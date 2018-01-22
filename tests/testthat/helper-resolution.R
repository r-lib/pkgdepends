
make_fake_deps <- function(...) {
  assertthat::assert_that(all_named(list(...)))

  d <- desc::desc("!new")
  if (length(list(...))) d$set(...)
  resolve_ref_deps(
    d$get_deps(),
    d$get("Remotes")[[1]],
    remotes_default_config()$dependencies)
}

make_fake_resolution1 <- function(ref, args) {
  pref <- parse_remotes(ref)[[1]]
  if (!is.null(args$extra)) pref[names(args$extra)] <- args$extra

  mirror <- args$mirror %||% remotes_default_config()$`cran-mirror`
  repodir <- args$repodir %||% "src/contrib"
  version <- args$version %||% "1.0.0"
  filename <- paste0(pref$package, "_", version, ".tar.gz")

  structure(
    list(
      files = list(list(
        source = c(
          sprintf("%s/%s/%s", mirror, repodir, filename),
          sprintf("%s/%s/Archive/%s/%s", mirror, repodir, pref$pakcvage,
                  filename)
        ),
        target = sprintf("src/contrib/%s_%s.tar.gz", pref$package, version),
        platform = args$platform %||% "source",
        rversion = args$rversion %||% "*",
        dir = repodir,
        package = pref$package,
        version = version,
        deps = args$deps %||% make_fake_deps(),
        status = args$status %||% "OK"
      )),
      remote = pref,
      status = args$status %||% "OK"
    ),
    class = c(paste0("remote_resolution_", pref$type), "remote_resolution")
  )
}

make_fake_metadata <- function() {
  list(
    resolution_start = Sys.time(),
    resolution_end = Sys.time()
  )
}

make_fake_resolution <- function(...) {
  pkgs <- list(...)
  assertthat::assert_that(all_named(pkgs))
  ress <- lapply_with_names(
    names(pkgs), function(n) make_fake_resolution1(n, pkgs[[n]]))

  direct <- vlapply(lapply(pkgs, "[[", "direct"), isTRUE)
  remotes__resolution_to_df(
    ress,
    make_fake_metadata(),
    parse_remotes(names(pkgs)[direct]),
    remotes_default_config()$cache_dir)
}

describe_fake_error <- function(pkgs, policy = "lazy") {
  lp <- remotes_i_create_lp_problem(pkgs, policy = policy)
  sol <- remotes_i_solve_lp_problem(lp)

  expect_true(sol$objval >= solve_dummy_obj - 1)
  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  describe_solution_error(pkgs, solution)
}
