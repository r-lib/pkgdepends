
test_that("new_pkg_installation_plan", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()
  lib <- withr::local_tempdir()
  lockfile <- tempfile()
  on.exit(unlink(lockfile), add = TRUE)

  config <- list(library = lib)
  prop <- pst(new_pkg_installation_proposal("pkg3", config = config))
  pst(prop$solve())

  prop$create_lockfile(lockfile)

  plan <- new_pkg_installation_plan(lockfile, config = config)
  expect_snapshot(plan)

  plan$update()

  expect_error(plan$resolve())
  expect_error(plan$async_resolve())
  expect_equal(plan$get_solve_policy(), NA_character_)
  expect_error(plan$set_solve_policy())
  expect_error(plan$solve())
})
