
solve_dummy_obj <- 1000000

remotes_solve <- function(self, private) {
  "!DEBUG starting to solve `length(private$resolution$packages)` packages"
  if (is.null(private$library)) {
    stop("No package library specified, see 'library' in new()")
  }
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  metadata <- list(solution_start = Sys.time())
  pkgs <- self$get_resolution()$data

  prb <- private$create_lp_problem(pkgs)
  sol <- private$solve_lp_problem(prb)

  if (sol$status != 0) {
    stop("Cannot solve installation, internal lpSolve error ", sol$status)
  }

  selected <- as.logical(sol$solution[seq_len(nrow(pkgs))])
  res <- list(
    status = if (sol$objval < solve_dummy_obj - 1) "OK" else "FAILED",
    data = private$subset_resolution(selected),
    problem = prb,
    solution = sol
  )

  metadata$solution_end <- Sys.time()
  res$data$metadata <- modifyList(res$data$metadata, metadata)
  class(res) <- unique(c("remotes_solution", class(res)))

  if (res$status == "FAILED") {
    res$failures <- describe_solution_error(pkgs, res)
  }

  private$solution$result <- res
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
#' 3. For all packages, the selected package must satisfy all refs
#'    for that package.
#'
#' And we want to minimize package downloads and package compilation:
#' 4. If a package is already installed, prefer the installed version,
#'    if possible.
#' 5. If a package is available as a binary, prefer the binary version,
#'    if possible.
#' 6. Can't install failed resolutions.
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
  remotes_i_create_lp_problem(pkgs)
}

## This is a separate function to make it testable without a `remotes`
## object.
##
## Variables:
## * 1:num are packages
## * (num+1):(num+num_pkgs) denote the relax variables for packages

remotes_i_create_lp_problem <- function(pkgs) {
  "!DEBUG creating LP problem"
  num <- nrow(pkgs)
  package_names <- unique(pkgs$package)
  num_pkgs <- length(package_names)
  lp <- structure(list(
    num = num + num_pkgs,
    num_pkgs = num_pkgs,
    conds = list(),
    pkgs = pkgs
  ), class = "remotes_lp_problem")

  ## Add a condition, for a subset of variables, with op and rhs
  cond <- function(vars, op = "<=", rhs = 1, type = NA_character_,
                   note = NULL) {
    lp$conds[[length(lp$conds)+1]] <<-
      list(vars = vars, op = op, rhs = rhs, type = type, note = note)
  }

  ## 4. & 5. coefficients of the objective function, this is very easy
  ## TODO: rule out incompatible platforms
  ## TODO: use rversion as well, for installed and binary packages
  lp$obj <- c(
    ifelse(pkgs$type == "installed", 0,
           ifelse(pkgs$platform == "source", 2, 1)),
    rep(solve_dummy_obj, num_pkgs)
  )

  ## 1. Each package exactly once
  ##    (We also add a dummy variable to catch errors.)
  for (p in seq_along(package_names)) {
    wh <- which(pkgs$package == package_names[p])
    cond(c(wh, num + p), op = "==", type = "exactly-once")
  }

  ## 2. All dependency versions must be satisfied
  ## We check the dependencies of each package candidate, and rule out
  ## unallowed combinations
  depconds <- function(wh) {
    if (pkgs$status[wh] != "OK") return()
    deps <- pkgs$dependencies[[wh]]
    deps <- deps[deps$version != "" & deps$ref != "R", ]
    for (i in seq_len(nrow(deps))) {
      deppkg <- deps$package[i]
      confl_pkgs <- which(pkgs$package == deppkg)
      for (co in confl_pkgs) {
        if (pkgs$status[co] == "OK" &&
            ! version_satisfies(pkgs$version[co], deps$op[i],
                                deps$version[i])) {
          note_txt <- paste(pkgs$version[co], deps$op[i], deps$version[i])
          note <- list(wh = wh, txt = note_txt)
          cond(c(wh, co), type = "dependency-version", note = note)
        }
      }
    }
  }
  lapply(seq_len(num), depconds)

  ## 3. For all packages, the selected package must satisfy all refs
  ##    for that package.
  satisfy <- function(wh) {
    pkgname <- pkgs$package[[wh]]
    res <- pkgs$resolution[[wh]]
    others <- setdiff(which(pkgs$package == pkgname), wh)
    for (o in others) {
      res2 <- pkgs$resolution[[o]]
      if (! isTRUE(satisfies_remote(res, res2))) {
        cond(o, op = "==", rhs = 0, type = "satisfy-refs", note = wh)
      }
    }
  }
  lapply(seq_len(num), satisfy)

  ## 7. Can't install failed resolutions
  failedconds <- function(wh) {
    if (pkgs$status[wh] != "FAILED") return()
    cond(wh, op = "==", rhs = 0, type = "ok-resolution")
  }
  lapply(seq_len(num), failedconds)

  lp
}

#' @export

print.remotes_lp_problem <- function(x, ...) {

  cat_cond <- function(cond) {
    if (cond$type == "dependency-version") {
      up <- x$pkgs$ref[cond$note$wh]
      down <- setdiff(x$pkgs$ref[cond$vars], up)
      txt <- cond$note$txt
      cat_line(" * `{up}` requirement `{down} {txt}` fails")

    } else if (cond$type == "satisfy-refs") {
      ref <- x$pkgs$ref[cond$note]
      cand <- x$pkgs$ref[cond$vars]
      cat_line(" * `{ref}` is not satisfied by `{cand}`")

    } else if (cond$type == "ok-resolution") {
      ref <- x$pkgs$ref[cond$vars]
      cat_line(" * `{ref}` resolution failed")

    } else if (cond$type == "exactly-once") {
      ## Do nothing

    } else {
      cat_line(" * Unknown condition")
    }
  }

  cat_line("LP problem for {x$num_pkgs} packages:")
  pn <- sort(unique(x$pkgs$package))
  cat_line(strwrap(paste(pn, collapse = ", "), indent = 2, exdent = 2))
  nc <- length(x$conds) - x$num_pkgs

  if (nc) {
    cat_line("Conditions:")
    lapply(x$conds, cat_cond)
  } else {
    cat_line("No conditions.")
  }

  invisible(x)
}

#' @importFrom lpSolve lp

remotes__solve_lp_problem <- function(self, private, problem) {
  res <- remotes_i_solve_lp_problem(problem)
  res
}

remotes_i_solve_lp_problem <- function(problem) {
  "!DEBUG solving LP problem"
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
  if (is.null(private$solution)) {
    stop("No solution found, need to call $solve()")
  }
  private$solution$result
}

remotes_install_plan <- function(self, private) {
  "!DEBUG creating install plan"
  sol <- self$get_solution()$data$data
  if (inherits(sol, "remotes_solve_error")) return(sol)

  deps <- lapply(sol$dependencies, "[[", "package")
  deps <- lapply(deps, setdiff, y = "R")
  installed <- ifelse(
    sol$type == "installed",
    file.path(private$library, sol$package),
    NA_character_)

  is_direct <- private$resolution$result$data$direct
  direct_packages <- private$resolution$result$data$package[is_direct]
  direct <- sol$direct |
    (sol$type == "installed" & sol$package %in% direct_packages)

  tibble::tibble(
    ref = sol$ref,
    package = sol$package,
    type = sol$type,
    version = sol$version,
    binary = sol$platform != "source",
    direct = direct,
    dependencies = I(deps),
    file = sol$fulltarget,
    installed = installed,
    metadata =
      lapply(sol$resolution, function(x) get_files(x)[[1]]$metadata)
  )
}

describe_solution_error <- function(pkgs, solution) {
  assert_that(
    ! is.null(pkgs),
    ! is.null(solution),
    solution$solution$objval >= solve_dummy_obj - 1L
  )

  sol <- solution$solution$solution

  ## 1. Which packages are problematic?
  num <- nrow(pkgs)
  package_names <- unique(pkgs$package)
  num_pkgs <- length(package_names)
  prob_pkgs <- package_names[which(sol[(num+1):(num+num_pkgs)] != 0)]

  ## 2. What constraints are they included in?
  unsat_conds <- lapply(prob_pkgs, function(p) {
    cands <- which(pkgs$package == p)
    which(vlapply(
      solution$problem$conds,
      function(cond) any(cands %in% cond$vars)
    ))
  })

  messages <- lapply(unsat_conds, function(cidxs) {
    unsat <- solution$problem$conds[cidxs]
    vcapply(unsat, function(cond) {
      if (is.na(cond$type)) {
        "internal error happened"

      } else if (cond$type == "exactly-once") {
        pkg <- unique(na.omit(pkgs$package[cond$vars]))
        glue("cannot install any version of package `{pkg}`")

      } else if (cond$type == "dependency-version") {
        other <- pkgs$package[[cond$note$wh]]
        pkg <- unique(na.omit(setdiff(pkgs$package[cond$vars], other)))
        otherdeps <- pkgs$dependencies[[cond$note$wh]]
        wh <- match(pkg, otherdeps$package)[1]
        ver <- paste(otherdeps$op[wh], otherdeps$version[wh])
        glue("`{pkg} {ver}`, required by `{other}`, cannot be installed")

      } else if (cond$type == "satisfy-refs") {
        ref <- pkgs$ref[cond$vars]
        ref2 <- pkgs$ref[cond$note]
        ## TODO: required by .....
        glue("`{ref}`, conflicts with `{ref2}`")

      } else if (cond$type == "ok-resolution") {
        ref <- pkgs$ref[cond$vars]
        glue("failed to resolve ref `{ref}`")

      } else {
        "internal error happened"
      }
    })
  })

  types <- vcapply(
    solution$problem$conds[unlist(unsat_conds)],
    "[[",
    "type")

  fails <- tibble(
    package = rep(prob_pkgs, viapply(unsat_conds, length)),
    constraint = unlist(unsat_conds),
    message = unlist(messages),
    type = types
  )
  class(fails) <- unique(c("remote_solution_error", class(fails)))

  fails
}

#' @export

format.remote_solution_error <- function(x, ...) {
  fails <- x
  if (!nrow(fails)) return()

  unlist(lapply(unique(fails$package), function(pkg) {
    xpkg <- fails[fails$package == pkg, ]
    msgs <- unique(na.omit(xpkg$message[xpkg$type != "exactly-once"]))
    c(glue("  * Cannot install package `{pkg}`."),
      if (length(msgs)) paste0("  - ", msgs))
  }))
}

#' @export

print.remote_solution_error <- function(x, ...) {
  cat(bold(red("Errors:")), sep = "\n")
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export

print.remotes_solution <- function(x, ...) {
  meta <- x$data$metadata
  data <- x$data$data

  direct <- unique(data$ref[data$direct])
  dt <- pretty_dt(meta$resolution_end - meta$resolution_start)
  dt2 <- pretty_dt(meta$solution_end - meta$solution_start)
  sol <- if (x$status == "OK") "SOLUTION" else "FAILED SOLUTION"
  head <- glue(
    "{logo()} {sol}, {length(direct)} refs, resolved in {dt}, ",
    "solved in {dt2} ")
  width <- getOption("width") - col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(symbol$line, max(width, 0)))
  if (x$status == "OK") {
    cat(blue(bold(head)), sep = "\n")
  } else {
    cat(red(bold(head)), sep = "\n")
  }

  print_refs(data, data$direct, header = NULL)

  print_refs(data, (! data$direct), header = "Dependencies", by_type = TRUE)

  if (!is.null(x$failures)) print(x$failures)

  invisible(x)
}
