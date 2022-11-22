
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

test_that("install package from GH, in subdir", {
  setup_fake_apps()
  setup_fake_gh_app()
  pkgcache::pkg_cache_delete_files()

  lib <- withr::local_tempdir()
  config <- list(library = lib)
  pkg <- new_pkg_installation_proposal("wesm/feather/R", config = config)
  suppressMessages(pkg$solve())
  lock <- file.path(withr::local_tempdir(), "pkg.lock")
  suppressMessages(pkg$create_lockfile(lock))

  plan <- new_pkg_installation_plan(lock, config = config)
  suppressMessages(plan$download())
  suppressMessages(plan$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # check cache state, must have a tree, a source and a binary package
  check_cache <- function() {
    cache <- pkgcache::pkg_cache_list()
    expect_equal(nrow(cache), 3L)
    expect_true(any(cache$package == "feather" & !cache$built))
    expect_true(any(
      cache$package == "feather" & cache$built & cache$platform == "source"
    ))
    expect_true(any(
      cache$package == "feather" & cache$built & cache$platform != "source"
    ))
  }
  check_cache()

  # install from cache, binary is selected
  remove.packages("feather", lib = lib)
  plan <- new_pkg_installation_plan(lock, config = config)
  suppressMessages(plan$download())
  expect_equal(
    plan$get_downloads()$download_status,
    paste("Had", current_r_platform())
  )
  suppressMessages(plan$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # cache is updated with the binary
  check_cache()

  # install from cache, no binary, source package is selected
  remove.packages("feather", lib = lib)
  pkgcache::pkg_cache_delete_files(
    built = TRUE, platform = current_r_platform()
  )
  plan <- new_pkg_installation_plan(lock, config = config)
  suppressMessages(plan$download())
  expect_equal(plan$get_downloads()$download_status, "Had")
  suppressMessages(plan$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # install from cache, no binary, no source, tree is selected
  remove.packages("feather", lib = lib)
  pkgcache::pkg_cache_delete_files(built = TRUE)
  plan <- new_pkg_installation_plan(lock, config = config)
  suppressMessages(plan$download())
  expect_equal(plan$get_downloads()$download_status, "Had")
  suppressMessages(plan$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # cache is updated with the source and binary
  check_cache()
})
