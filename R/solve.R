
solve_dummy_obj <- 1000000

remotes_solve <- function(self, private, policy) {
  "!DEBUG starting to solve `length(private$resolution$packages)` packages"
  if (is.null(private$library)) {
    stop("No package library specified, see 'library' in new()")
  }
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  metadata <- list(solution_start = Sys.time())
  pkgs <- self$get_resolution()$data

  prb <- private$create_lp_problem(pkgs, policy)
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

remotes_stop_for_solve_error <- function(self, private) {
  if (is.null(private$solution)) {
    stop("No solution found, need to call $solve()")
  }

  sol <- self$get_solution()

  if (sol$status != "OK") {
    msg <- paste(format(sol$failures), collapse = "\n")
    stop("Cannot install packages:\n", msg, call. = FALSE)
  }
}

#' Create the LP problem that solves the installation
#'
#' Each row in the resolution data frame is an installation candidate.
#' Each row corresponds to a binary variable \eqn{p_i}{p[i]}, which is
#' 1 if that package will be installed.
#'
#' TODO
#'
#' And we want to minimize package downloads and package compilation:
#' 6. If a package is already installed, prefer the installed version,
#'    if possible.
#' 7. If a package is available as a binary, prefer the binary version,
#'    if possible.
#'
#' We do this by assigning cost 1 to installed versions, cost 2 to
#' binary packages, and cost 3 to source packages. Then we minimize the
#' total cost, while satisfying the constraints.
#'
#' Other cost schemes will be added later.
#'
#' @param pkgs Resolution data frame, that contains the locally installed
#'   packages as well.
#' @param policy Version selection policy.
#' @return An S3 object for a linear (integer) optimization problem,
#'   to be used with [lpSolve::lp()] (eventually).
#'
#' @keywords internal

remotes__create_lp_problem <- function(self, private, pkgs, policy) {
  remotes_i_create_lp_problem(pkgs, policy)
}

## This is a separate function to make it testable without a `remotes`
## object.
##
## Variables:
## * 1:num are candidates
## * (num+1):(num+num_direct_pkgs) are the relax variables for direct refs

remotes_i_create_lp_problem <- function(pkgs, policy) {
  "!DEBUG creating LP problem"
  num_candidates <- nrow(pkgs)
  packages <- unique(pkgs$package)
  direct_packages <- unique(pkgs$package[pkgs$direct])
  indirect_packages <- setdiff(packages, direct_packages)
  num_direct <- length(direct_packages)

  lp <- structure(list(
    num_candidates = num_candidates,
    num_direct = num_direct,
    total = num_candidates + num_direct,
    conds = list(),
    pkgs = pkgs
  ), class = "remotes_lp_problem")

  ## Add a condition, for a subset of variables, with op and rhs
  cond <- function(vars, op = "<=", rhs = 1, coef = rep(1, length(vars)),
                   type = NA_character_, note = NULL) {
    lp$conds[[length(lp$conds)+1]] <<-
      list(vars = vars, coef = coef, op = op, rhs = rhs, type = type,
           note = note)
  }

  ## Coefficients of the objective function, this is very easy
  ## TODO: rule out incompatible platforms
  ## TODO: use rversion as well, for installed and binary packages
  if (policy == "lazy") {
    ## Simple: installed < binary < source
    lp$obj <- ifelse(pkgs$type == "installed", 1,
              ifelse(pkgs$platform == "source", 3, 2))

  } else if (policy == "upgrade") {
    ## Sort the candidates of a package according to version number
    lp$obj <- rep(num_candidates + 1L, num_candidates)
    whpp <- pkgs$status == "OK" & !is.na(pkgs$version)
    pn <- unique(pkgs$package[whpp])
    for (p in pn) {
      whp <-  whpp & pkgs$package == p
      v <- pkgs$version[whp]
      lp$obj[whp] <- order(package_version(v), decreasing=TRUE)
    }

  } else {
    stop("Unknown version selection policy")
  }

  lp$obj <- c(lp$obj, rep(solve_dummy_obj, num_direct))

  ## 1. Each directly specified package exactly once.
  ##    (We also add a dummy variable to catch errors.)
  for (p in seq_along(direct_packages)) {
    pkg <- direct_packages[p]
    wh <- which(pkgs$package == pkg)
    cond(c(wh, num_candidates + p), op = "==", type = "exactly-once")
  }

  ## 2. Each non-direct package must be installed at most once
  for (p in seq_along(indirect_packages)) {
    pkg <- indirect_packages[p]
    wh <- which(pkgs$package == pkg)
    cond(wh, op = "<=", type = "at-most-once")
  }

  ## 3. Direct refs must be satisfied
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
  lapply(seq_len(num_candidates)[pkgs$direct], satisfy)

  ## 4. Package dependencies must be satisfied
  depconds <- function(wh) {
    if (pkgs$status[wh] != "OK") return()
    deps <- pkgs$dependencies[[wh]]
    deps <- deps[deps$ref != "R", ]
    for (i in seq_len(nrow(deps))) {
      depref <- deps$ref[i]
      depver <- deps$version[i]
      depop  <- deps$op[i]
      deppkg <- deps$package[i]
      ## See which candidate satisfies this ref
      res <- pkgs$resolution[[match(depref, pkgs$ref)]]
      cand <- which(pkgs$package == deppkg)
      good_cand <- Filter(
        x = cand,
        function(c) {
          candver <- pkgs$version[c]
          isTRUE(satisfies_remote(res, pkgs$resolution[[c]])) &&
            (depver == "" || version_satisfies(candver, depop, depver))
        })
      bad_cand <- setdiff(cand, good_cand)

      report <- c(
        if (length(good_cand)) {
          gc <- paste(pkgs$ref[good_cand], pkgs$version[good_cand])
          paste0("version ", paste(gc, collapse = ", "))
        },
        if (length(bad_cand)) {
          bc <- paste(pkgs$ref[bad_cand], pkgs$version[bad_cand])
          paste0("but not ", paste(bc, collapse = ", "))
        },
        if (! length(cand)) "but no candidates"
      )
      txt <- glue("{pkgs$ref[wh]} depends on {depref}: \\
                   {collapse(report, sep = ', ')}")
      note <- list(wh = wh, ref = depref, cand = cand,
                   good_cand = good_cand, txt = txt)

      cond(
        c(wh, good_cand), "<=", rhs = 0,
        coef = c(1, rep(-1, length(good_cand))),
        type = "dependency", note = note
      )
    }
  }
  lapply(seq_len(num_candidates), depconds)

  ## 5. Can't install failed resolutions
  failedconds <- function(wh) {
    if (pkgs$status[wh] != "FAILED") return()
    cond(wh, op = "==", rhs = 0, type = "ok-resolution")
  }
  lapply(seq_len(num_candidates), failedconds)

  lp
}

#' @export

print.remotes_lp_problem <- function(x, ...) {

  cat_cond <- function(cond) {
    if (cond$type == "dependency") {
      cat_line(" * {cond$note$txt}")

    } else if (cond$type == "satisfy-refs") {
      ref <- x$pkgs$ref[cond$note]
      cand <- x$pkgs$ref[cond$vars]
      cat_line(" * `{ref}` is not satisfied by `{cand}`")

    } else if (cond$type == "ok-resolution") {
      ref <- x$pkgs$ref[cond$vars]
      cat_line(" * `{ref}` resolution failed")

    } else if (cond$type == "exactly-once") {
      ## Do nothing

    } else if (cond$type == "at-most-once") {
      ## Do nothing

    } else {
      cat_line(" * Unknown condition")
    }
  }

  cat_line("LP problem for {x$num_candidates} refs:")
  pn <- sort(x$pkgs$ref)
  cat_line(strwrap(paste(pn, collapse = ", "), indent = 2, exdent = 2))
  nc <- length(x$conds) - x$num_direct

  if (nc > 0) {
    cat_line("Constraints:")
    lapply(x$conds, cat_cond)
  } else {
    cat_line("No constraints")
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
  condmat <- matrix(0, nrow = length(problem$conds), ncol = problem$total)
  for (i in seq_along(problem$conds)) {
    cond <- problem$conds[[i]]
    condmat[i, cond$vars] <- cond$coef
  }

  dir <- vcapply(problem$conds, "[[", "op")
  rhs <- vapply(problem$conds, "[[", "rhs", FUN.VALUE = double(1))
  lp("min", problem$obj, condmat, dir, rhs, int.vec = seq_len(problem$total))
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

  binary = sol$platform != "source"
  vignettes <- ! binary & ! sol$type %in% c("cran", "bioc", "standard")

  tibble::tibble(
    ref = sol$ref,
    package = sol$package,
    type = sol$type,
    version = sol$version,
    binary = binary,
    direct = direct,
    dependencies = I(deps),
    file = sol$fulltarget,
    installed = installed,
    vignettes = vignettes,
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

  num <- nrow(pkgs)
  if (!num) stop("No solution errors to describe")
  sol <- solution$solution$solution
  sol_pkg <- sol[1:num]
  sol_dum <- sol[(num+1):solution$problem$total]

  ## For each candidate, we work out if it _could_ be installed, and if
  ## not, why not. Possible cases:
  ## 1. it is is in the install plan, so it can be installed, YES
  ## 2. it failed resolution, so NO
  ## 3. it does not satisfy a direct ref for the same package, so NO
  ## 4. it conflicts with another to-be-installed candidate of the
  ##    same package, so NO
  ## 5. one of its (downstream) dependencies cannot be installed, so NO
  ## 6. otherwise YES

  FAILS <- c("failed-res", "satisfy-direct", "conflict", "dep-failed")

  state <- rep("maybe-good", num)
  note <- replicate(num, NULL)
  downstream <- replicate(num, character(), simplify = FALSE)

  state[sol_pkg == 1] <- "installed"

  ## Candidates that failed resolution
  cnd <- solution$problem$conds
  typ <- vcapply(cnd, "[[", "type")
  var <- lapply(cnd, "[[", "vars")
  fres_vars <- unlist(var[typ == "ok-resolution"])
  state[fres_vars] <- "failed-res"
  for (fv in fres_vars) {
    note[[fv]] <- c(note[[fv]], unname(get_error_message(pkgs$resolution[[fv]])))
  }

  ## Candidates that conflict with a direct package
  for (w in which(typ == "satisfy-refs")) {
    sv <- var[[w]]
    down <- pkgs$ref[sv]
    up <- pkgs$ref[cnd[[w]]$note]
    state[sv] <- "satisfy-direct"
    note[[sv]] <- c(note[[sv]], glue("Conflicts {up}"))
  }

  ## Find "conflict". These are candidates that are not installed,
  ## and have an "at-most-once" constraint with another package that will
  ## be installed. So we just go over these constraints.
  for (c in cnd[typ == "at-most-once"]) {
    is_in <- sol_pkg[c$vars] != 0
    if (any(is_in)) {
      state[c$vars[!is_in]] <- "conflict"
      package <- pkgs$package[c$vars[1]]
      inst <- pkgs$ref[c$vars[is_in]]
      vv <- c$vars[!is_in]
      for (v in vv) {
        note[[v]] <- c(
          note[[v]],
          glue("{pkgs$ref[v]} conflict with {inst}, to be installed"))
      }
    }
  }

  ## Find "dep-failed". This is the trickiest. First, if there are no
  ## condidates at all
  type_dep <- typ == "dependency"
  dep_up <- viapply(cnd[type_dep], function(x) x$vars[1])
  dep_cands <- lapply(cnd[type_dep], function(x) x$vars[-1])
  no_cands <- which(! viapply(dep_cands, length) &
                    state[dep_up] == "maybe-good")
  for (x in no_cands) {
    pkg <- cnd[type_dep][[x]]$note$ref
    state[dep_up[x]] <- "dep-failed"
    note[[ dep_up[x] ]] <-
      c(note[[ dep_up[x] ]], glue("Cannot install dependency {pkg}"))
    downstream[[ dep_up[x] ]] <- c(downstream[[ dep_up[x] ]], pkg)
  }

  ## Then we start with the already known
  ## NO answers, and see if they rule out upstream packages
  new <- which(state %in% FAILS)
  while (length(new)) {
    dep_cands <- lapply(dep_cands, setdiff, new)
    which_new <- which(!viapply(dep_cands, length) & state[dep_up] == "maybe-good")
    for (x in which_new) {
      pkg <- cnd[type_dep][[x]]$note$ref
      state[ dep_up[x] ] <- "dep-failed"
      note[[ dep_up[x] ]] <- c(
        note[[ dep_up[x] ]], glue("Cannot install dependency {pkg}"))
      downstream[[ dep_up[x] ]] <- c(downstream[[ dep_up[x] ]], pkg)
    }
    new <- dep_up[which_new]
  }

  ## The rest is good
  state[state == "maybe-good"] <- "could-be"

  wh <- state %in% FAILS
  fails <- pkgs[wh, ]
  fails$failure_type <- state[wh]
  fails$failure_message <-  note[wh]
  fails$failure_down <- downstream[wh]
  class(fails) <- unique(c("remote_solution_error", class(fails)))

  fails
}

#' @export

format.remote_solution_error <- function(x, ...) {
  fails <- x
  if (!nrow(fails)) return()

  done <- rep(FALSE, nrow(x))
  res <- character()

  do <- function(i) {
    if (done[i]) return()
    done[i] <<- TRUE
    msgs <- unique(fails$failure_message[[i]])
    res <<- c(
      res, glue("  * Cannot install `{fails$ref[i]}`."),
      if (length(msgs)) paste0("    - ", msgs)
    )
    lapply(match(fails$failure_down[[i]], fails$ref), do)
  }

  direct_refs <- which(fails$direct)
  lapply(direct_refs, do)

  res
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
