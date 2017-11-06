
remotes_solve <- function(self, private) {
  if (is.null(private$library)) {
    stop("No package library specified, see 'library' in new()")
  }
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  pkgs <- self$get_resolution()

  prb <- private$create_lp_problem(pkgs)
  sol <- private$solve_lp_problem(prb)

  packages <- private$subset_resolution(as.logical(sol$solution))
  private$solution <- list(
    packages = packages,
    result = private$resolution_to_df(packages),
    problem = prb,
    solution = sol
  )

  self$get_solution()
}

#' Create the LP problem that solves the installation
#'
#' Each row in the resolution data frame is an installation candidate.
#' Each row corresponds to a binary variable \eqn{p_i}{p[i]}, which is
#' 1 if that package will be installed.
#'
#' The constraints we have:
#' 1. For each package \eqn{k}, we need exactly one candidate to be
#'    installed: \eqn{\sum_{i\in k} p_i=1}{sum(p[i], i in k) = 1}.
#' 2. All dependency versions must be satisfied.
#' 3. If the ref of a package is directly specified, the package must
#'    satisfy the ref.
#'
#' And we want to minimize package downloads and package compilation:
#' 4. If a package is already installed, prefer the installed version,
#'    if possible.
#' 5. If a package is available as a binary, prefer the binary version,
#'    if possible.
#' We do this by assigning cost 0 to installed versions, cost 1 to
#' binary packages, and cost 2 to source packages. Then we minimize the
#' total cost, while satisfying the constraints.
#'
#' @param pkgs Resolution data frame, that contains the locally installed
#'   packages as well.
#' @return An S3 object for a linear (integer) optimization problem,
#'   to be used with [lpSolve::lp()] (eventually).
#'
#' @keywords internal

remotes__create_lp_problem <- function(self, private, pkgs) {
  num <- nrow(pkgs)
  lp <- list(num = num, conds = list())

  ## Add a condition, for a subset of variables, with op and rhs
  cond <- function(vars, op = "<=", rhs = 1) {
    lp$conds[[length(lp$conds)+1]] <<- list(vars = vars, op = op, rhs = rhs)
  }

  ## 4. & 5. coefficients of the objective function, this is very easy
  ## TODO: use rversion as well, for installed and binary packages
  my_platform <- current_r_platform()
  lp$obj <- ifelse(pkgs$type == "installed", 0,
                   ifelse(pkgs$platform == my_platform, 1, 2))

  ## 1. Each package exactly once
  tapply(seq_len(num), pkgs$package, cond, op = "==")

  ## 2. All dependency versions must be satisfied
  ## We check the dependencies of each package candidate, and rule out
  ## unallowed combinations
  depconds <- function(wh) {
    if (pkgs$status[wh] != "OK") return()
    deps <- pkgs$dependencies[[wh]]
    deps <- deps[deps$version != "", ]
    for (i in seq_len(nrow(deps))) {
      deppkg <- deps$package[i]
      confl_pkgs <- which(pkgs$package == deppkg)
      for (co in confl_pkgs) {
        if (pkgs$status[co] == "OK" &&
            ! version_satisfies(pkgs$version[co], deps$op[i],
                                deps$version[i])) {
          cond(c(wh, co))
        }
      }
    }
  }
  lapply(seq_len(num), depconds)

  ## 3. If the ref of a package is directly specified, the package must
  ## satisfy the ref.
  directs <- function(wh) {
    pkgname <- pkgs$package[[wh]]
    remote <- pkgs$remote[[wh]]
    res <- private$resolution$packages[[ pkgs$res_id[wh] ]]
    others <- setdiff(which(pkgs$package == pkgname), wh)
    for (o in others) {
      res2 <- private$resolution$packages[[  pkgs$res_id[o] ]]
      if (! isTRUE(satisfies_remote(res, res2))) {
        cond(c(wh, o))
      }
    }
  }
  lapply(which(vlapply(pkgs$direct, isTRUE)), directs)

  ## 6. Can't install failed resolutions
  failedconds <- function(wh) {
    if (pkgs$status[wh] != "FAILED") return()
    cond(wh, op = "==", rhs = 0)
  }
  lapply(seq_len(num), failedconds)

  lp
}

#' @importFrom lpSolve lp

remotes__solve_lp_problem <- function(self, private, problem) {
  condmat <- matrix(0, nrow = length(problem$conds), ncol = problem$num)
  for (i in seq_along(problem$conds)) {
    cond <- problem$conds[[i]]
    condmat[i, cond$vars] <- 1
  }

  dir <- vcapply(problem$conds, "[[", "op")
  rhs <- vapply(problem$conds, "[[", "rhs", FUN.VALUE = double(1))
  lp("min", problem$obj, condmat, dir, rhs, int.vec = seq_len(problem$num))
}

remotes_get_solution <- function(self, private) {
  if (is.null(private$solution$result)) {
    stop("No solution found, need to call $solve()")
  }
  private$solution$result
}

remotes_install_plan <- function(self, private) {
  sol <- self$get_solution()
  deps <- lapply(sol$dependencies, "[[", "package")
  tibble::tibble(
    package = sol$package,
    type = sol$type,
    version = sol$version,
    binary = sol$platform != "source",
    direct = sol$direct,
    dependencies = I(deps),
    file = sol$fulltarget
  )
}
