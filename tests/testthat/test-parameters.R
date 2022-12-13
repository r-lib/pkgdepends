
test_that("reinstall, standard", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  lib <- withr::local_tempdir("pkgdepends-")
  config <- list(library = lib)
  p <- new_pkg_installation_proposal("pkg3", config = config)
  suppressMessages(p$solve())
  suppressMessages(p$download())
  suppressMessages(p$install())

  p <- new_pkg_installation_proposal("pkg1", config = config)
  suppressMessages(p$solve())
  expect_snapshot({
    "pkg1 is already installed"
    p$get_solution()
  }, transform = transform_installed_in_temp)
  
  p <- new_pkg_installation_proposal("pkg3?reinstall", config = config)
  suppressMessages(p$solve())
  expect_snapshot({
    "request a reinstall"
    p$get_solution()
  }, transform = transform_installed_in_temp)

  p <- new_pkg_installation_proposal(
    c("pkg3", "pkg1=?reinstall"),
    config = config
  )
  suppressMessages(p$solve())
  expect_snapshot({
    "request a reinstall of a dependency"
    p$get_solution()
  }, transform = transform_installed_in_temp)

  p <- new_pkg_installation_proposal(
    c("pkg3", "pkg1?reinstall"),
    config = config
  )
  suppressMessages(p$solve())
  expect_snapshot({
    "one reinstall, one not"
    p$get_solution()
  }, transform = transform_installed_in_temp)
})

test_that("reinstsll from URL", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  lib <- withr::local_tempdir("pkgdepends-")
  config <- list(library = lib)
  p <- new_pkg_installation_proposal("pkg3", config = config)
  suppressMessages(p$solve())
  suppressMessages(p$download())
  suppressMessages(p$install())

  url <- paste0(
    fake_cran$url(),
    "/src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz"
  )
  p <- new_pkg_installation_proposal(
    paste0("url::", url, "?reinstall"),
    config = config
  )
  suppressMessages(p$solve())
  expect_snapshot({
    "reinstall from direct URL"
    p$get_solution()
  }, transform = function(x) transform_local_port(transform_installed_in_temp(x)))

  p <- new_pkg_installation_proposal(
    c("pkg1=?reinstall", paste0("url::", url)),
    config = config
  )
  suppressMessages(p$solve())
  expect_snapshot({
    "reinstall from URL, extra parameters"
    p$get_solution()
  }, transform = function(x) transform_local_port(transform_installed_in_temp(x)))
})

test_that("source", {
  repo <- dcf("
    Package: pkg
    Imports: pkg2

    Package: pkg2
  ")

  setup_fake_apps(
    cran_repo = repo,
    cran_options = list(platforms = c("windows", "source"))
  )

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg?source",
    config = list(
      dependencies = TRUE,
      library = tempfile(),
      platforms = c("x86_64-w64-mingw32", "source")
    )
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  sol <- p$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("package", "platform")])
})

test_that("source for dependency", {
  repo <- dcf("
    Package: pkg
    Imports: pkg2

    Package: pkg2
  ")

  setup_fake_apps(
    cran_repo = repo,
    cran_options = list(platforms = c("windows", "source"))
  )

  p <- suppressMessages(new_pkg_installation_proposal(
    c("pkg", "pkg2=?source"),
    config = list(
      dependencies = TRUE,
      library = tempfile(),
      platforms = c("x86_64-w64-mingw32", "source")
    )
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  sol <- p$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("package", "platform")])
})
