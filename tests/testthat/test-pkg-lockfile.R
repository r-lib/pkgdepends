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

  expect_snapshot(error = TRUE, {
    plan$resolve()
    plan$async_resolve()
  })
  expect_equal(plan$get_solve_policy(), NA_character_)
  expect_snapshot(error = TRUE, {
    plan$set_solve_policy()
    plan$solve()
  })
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
    built = TRUE,
    platform = current_r_platform()
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

test_that("sysreqs", {
  # Not the best test, because there are no system requirements to update...
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
  plan$update_sysreqs()

  expect_snapshot(error = TRUE, {
    plan$resolve()
    plan$async_resolve()
  })
  expect_equal(plan$get_solve_policy(), NA_character_)
  expect_snapshot(error = TRUE, {
    plan$set_solve_policy()
    plan$solve()
  })
})

test_that("install_sysreqs", {
  setup_fake_apps()
  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  config <- list(
    library = lib,
    sysreqs = TRUE,
    sysreqs_platform = "aarch64-unknown-linux-gnu-ubuntu-22.04",
    sysreqs_lookup_system = FALSE,
    sysreqs_dry_run = TRUE,
    sysreqs_sudo = FALSE
  )
  prop <- new_pkg_installation_proposal("curl", config = config)
  suppressMessages(prop$solve())
  lock <- tempfile(fileext = ".lock")
  on.exit(unlink(lock), add = TRUE)
  prop$create_lockfile(path = lock)

  plan <- new_pkg_installation_plan(lock)
  expect_snapshot(
    {
      plan$show_solution()
      plan$show_sysreqs()
    },
    transform = transform_bytes
  )
})

test_that("update_sysreqs", {
  setup_fake_apps()
  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  config <- list(
    library = lib,
    sysreqs = TRUE,
    sysreqs_platform = "aarch64-unknown-linux-gnu-ubuntu-22.04",
    sysreqs_lookup_system = FALSE,
    sysreqs_dry_run = TRUE,
    sysreqs_sudo = FALSE
  )
  prop <- new_pkg_installation_proposal("curl", config = config)
  suppressMessages(prop$solve())
  lock <- tempfile(fileext = ".lock")
  on.exit(unlink(lock), add = TRUE)
  prop$create_lockfile(path = lock)

  plan <- new_pkg_installation_plan(lock, config = config)
  iplan <- get_private(plan)$plan

  # inject system package data
  fake <- data_frame(
    status = "ii",
    package = c("libssl-dev", "libssl3"),
    version = "3.0.2-0ubuntu1.10",
    provides = list(list())
  )
  old <- iplan$.__enclos_env__$private$solution$result$data$sysreqs_packages
  new <- sysreqs_update_state(old, fake)
  iplan$.__enclos_env__$private$solution$result$data$sysreqs_packages <- new
  expect_snapshot({
    plan$show_sysreqs()
  })
})

test_that("update_sysreqs with old lock file", {
  setup_fake_apps()
  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  # does nothing if no sysreqs_packages
  config <- list(
    library = lib,
    sysreqs = FALSE,
    sysreqs_platform = "unknown",
    sysreqs_lookup_system = FALSE
  )
  prop <- new_pkg_installation_proposal("curl", config = config)
  suppressMessages(prop$solve())
  lock <- tempfile(fileext = ".lock")
  on.exit(unlink(lock), add = TRUE)
  prop$create_lockfile(path = lock)

  config$sysreqs <- TRUE
  plan <- new_pkg_installation_plan(lock, config = config)
  iplan <- get_private(plan)$plan

  pkgplan_update_sysreqs(iplan, get_private(iplan))
  expect_snapshot({
    plan$show_sysreqs()
  })
})
