
remotes_solve <- function(self, private, lib) {
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  inst <- resolve_installed(lib)
  ress <- self$get_resolution()
  inst <- inst[inst$package %in% ress$package, ]

  pkgs <- rbind(inst, ress)
  pkgs <- pkgs[order(pkgs$package), ]

  prb <- create_lp_problem(pkgs)
  sol <- solve_lp_problem(prb)
  browser()
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
#'   to be used with [lpsolve::lp()] (eventually).
#'
#' @keywords internal

create_lp_problem <- function(pkgs) {
  num <- nrow(pkgs)
  lp <- list(num = num, conds = list())

  ## Add a condition, for a subset of variables, with op and rhs
  cond <- function(vars, op = "<=", rhs = 1) {
    lp$conds[[length(lp$conds)+1]] <<- list(vars = vars, op = op, rhs = rhs)
  }

  ## 4. & 5. coefficients of the objective function, this is very easy
  ## TODO: use rversion as well, for installed and binary packages
  my_platform <- current_r_platform()
  lp$obj <- ifelse(pkgs$status == "INSTALLED", 0,
                   ifelse(pkgs$platform == my_platform, 1, 2))

  ## 1. Each package exactly once
  tapply(seq_len(num), pkgs$package, cond, op = "==")

  ## 2. All dependency versions must be satisfied
  ## We check the dependencies of each package candidate, and rule out
  ## unallowed combinations
  depconds <- function(wh) {
    deps <- pkgs$dependencies[[wh]]
    deps <- deps[deps$version != "", ]
    for (i in seq_len(nrow(deps))) {
      deppkg <- deps$package[i]
      confl_pkgs <- which(pkgs$package == deppkg)
      for (co in confl_pkgs) {
        if (! version_satisfies(pkgs$version[co], deps$op[i],
                                deps$version[i])) {
          cond(c(wh, co))
        }
      }
    }
  }
  lapply(seq_len(num), depconds)

  ## 3. If the ref of a package is directly specified, the package must
  ## satisfy the ref.
  ## This is a TODO
  directs <- function(wh) {
    pkgname <- pkgs$package[wh]
    others <- which(pkgs$package == pkgname)
    ## TODO
  }
  lapply(which(vlapply(pkgs$direct, isTRUE)), directs)

  lp
}

#' @importFrom lpSolve lp

solve_lp_problem <- function(problem) {
  condmat <- matrix(0, nrow = length(problem$conds), ncol = problem$num)
  for (i in seq_along(problem$conds)) {
    cond <- problem$conds[[i]]
    condmat[i, cond$vars] <- 1
  }

  dir <- vcapply(problem$conds, "[[", "op")
  rhs <- vapply(problem$conds, "[[", "rhs", FUN.VALUE = double(1))
  lp("min", problem$obj, condmat, dir, rhs, int.vec = seq_len(problem$num))
}
