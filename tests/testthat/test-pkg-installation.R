
test_that("new_pkg_installation_proposal", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = tempfile())
  ))

  expect_snapshot(prop)
  expect_snapshot(prop$get_refs())
  expect_snapshot(sort(prop$get_config()$list()))

  suppressMessages(prop$resolve())
  expect_snapshot(
    prop$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
  )

  expect_snapshot(prop$get_solve_policy())
  prop$set_solve_policy("upgrade")
  expect_snapshot(prop$get_solve_policy())

  suppressMessages(prop$solve())
  expect_silent(prop$stop_for_solution_error())
  expect_snapshot(prop$get_solution())
  expect_snapshot(
    prop$show_solution(),
    transform = transform_bytes
  )

  expect_snapshot(
    prop$draw(),
    transform = transform_bytes
  )
})

test_that("async_resolve", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = tempfile())
  ))

  expect_snapshot(
    suppressMessages(synchronize(
      prop$async_resolve()$then(function() "done")
    ))
  )
  expect_snapshot(
    prop$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
  )
})

test_that("create_lockfile", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = tempfile())
  ))

  suppressMessages(prop$solve())
  tmp <- tempfile(fileext = ".lock")
  on.exit(unlink(tmp), add = TRUE)

  suppressMessages(prop$create_lockfile(tmp))
  expect_error(lock <- jsonlite::fromJSON(tmp), NA)

  lock$packages$filesize <- 100

  # Newer windows adds a --no-multiarch install parameter, by default
  vrt <- if (.Platform$OS.type == "windows" && getRversion() >= "4.2.0") {
    "windows"
  } else {
    "unix"
  }
  expect_snapshot(
    lock$packages,
    transform = function(x) transform_local_port(transform_sha(x)),
    variant = vrt
  )
})

test_that("download", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = tempfile())
  ))

  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  suppressMessages(prop$download())
  expect_snapshot(
    prop$get_downloads()[, c("target", "error")]
  )
  expect_silent(prop$stop_for_download_error())
})

test_that("async_download", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = tempfile())
  ))

  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  suppressMessages(synchronize(prop$async_download()))
  expect_snapshot(
    prop$get_downloads()[, c("target", "error")]
  )
  expect_silent(prop$stop_for_download_error())
})

test_that("install", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  lib <- withr::local_tempdir()
  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = lib)
  ))

  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  suppressMessages(prop$download())
  suppressMessages(prop$install())

  expect_snapshot(
    pkgcache::parse_installed(lib)[, c("Package", "RemoteRepos")],
    transform = transform_local_port
  )
})

test_that("install_sysreqs", {
  # TODO
})

test_that("get_install_plan", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  lib <- withr::local_tempdir()
  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = lib)
  ))

  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  suppressMessages(prop$download())
  expect_snapshot(
    prop$get_install_plan()[, c("package", "direct", "dependencies")]
  )
  expect_snapshot(
    prop$get_install_plan()[["dependencies"]]
  )
})
