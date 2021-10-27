
make_fake_deps <- function(...) {
  assert_that(all_named(list(...)))

  d <- desc::desc("!new")
  if (length(list(...))) d$set(...)
  resolve_ref_deps(
    d$get_deps(),
    d$get("Remotes")[[1]],
    NULL
  )
}

make_fake_resolution1 <- function(ref, args = list()) {
  pref <- parse_pkg_refs(ref)[[1]]
  if (!is.null(args$extra)) pref[names(args$extra)] <- args$extra

  mirror <- args$mirror %||% current_config()$get("cran-mirror")
  repodir <- args$repodir %||% "src/contrib"
  version <- args$version %||% "1.0.0"
  filename <- paste0(pref$package, "_", version, ".tar.gz")

  def <- list(
    ref = ref,
    type = pref$type,
    package = pref$package,
    version = version,
    sources = c(
      sprintf("%s/%s/%s", mirror, repodir, filename),
      sprintf("%s/%s/Archive/%s/%s", mirror, repodir, pref$package,
              filename)
    ),
    dep_types = tolower(pkg_dep_types_hard())
  )

  modifyList(def, args)
}

make_fake_metadata <- function() {
  list(
    resolution_start = Sys.time(),
    resolution_end = Sys.time()
  )
}

make_fake_resolution <- function(...) {
  pkgs <- list(...)
  assert_that(all_named(pkgs))
  ress <- lapply_with_names(
    names(pkgs), function(n) make_fake_resolution1(n, pkgs[[n]]))

  res  <- res_make_empty_df()
  for (r in ress) res <- res_add_df_entries(res, r)
  res
}

describe_fake_error <- function(pkgs, policy = "lazy",
                                config = current_config()) {
  lp <- pkgplan_i_create_lp_problem(pkgs, config, policy)
  sol <- pkgplan_i_solve_lp_problem(lp)

  expect_true(sol$objval >= solve_dummy_obj - 1)
  solution <- list(status = "FAILED", data = pkgs, problem = lp,
                   solution = sol)
  describe_solution_error(pkgs, solution)
}
