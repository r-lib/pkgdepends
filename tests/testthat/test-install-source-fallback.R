# Source package instead of binary: https://github.com/r-lib/pak/issues/891
make_source_fallback_repo <- function() {
  pc <- asNamespace("pkgcache")
  repo <- withr::local_tempdir(.local_envir = parent.frame())
  pkgs <- pc$dcf("Package: foo\nVersion: 0.0.0.9000\nLicense: GPL-3\n")
  pc$make_dummy_repo(
    repo,
    pkgs,
    options = list(platforms = c("windows", "source"), no_metadata = TRUE)
  )

  # Overwrite the Windows binary with the source tarball
  src <- dir(
    file.path(repo, "src", "contrib"),
    pattern = "^foo_.*\\.tar\\.gz$",
    full.names = TRUE
  )
  bin <- dir(
    file.path(repo, "bin"),
    pattern = "^foo_.*\\.zip$",
    full.names = TRUE,
    recursive = TRUE
  )
  file.copy(src, bin, overwrite = TRUE)

  repo
}

test_that("a source package served in place of a binary (pak#891)", {
  skip_on_cran()
  skip_on_os("windows")

  repo <- make_source_fallback_repo()
  app <- webfakes::local_app_process(
    local({
      a <- webfakes::new_app()
      a$use(webfakes::mw_static(root = repo))
      a
    }),
    opts = webfakes::server_opts(num_threads = 3)
  )
  setup_fake_apps(cran_app = app)

  lib <- withr::local_tempdir()
  prop <- suppressMessages(new_pkg_installation_proposal(
    "foo",
    config = list(
      library = lib,
      dependencies = FALSE,
      # Force the Windows binary to be picked, independently of the test host.
      platforms = c("x86_64-w64-mingw32", "source")
    )
  ))
  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  prop$stop_for_solution_error()

  # The metadata makes pkgdepends believe this is a binary package.
  sol <- prop$get_solution()$data
  expect_identical(sol$package, "foo")
  expect_false(sol$platform == "source")

  suppressMessages(prop$download())
  prop$stop_for_download_error()

  # The install plan marks it as a binary, so it will be installed directly
  plan <- prop$get_install_plan()
  expect_true(plan$binary[plan$package == "foo"])

  # But the downloaded "binary" is really a source package
  file <- plan$file[plan$package == "foo"]
  extracted <- withr::local_tempdir()
  utils::untar(file, exdir = extracted)
  dsc <- desc::desc(file.path(extracted, "foo", "DESCRIPTION"))
  expect_false(dsc$has_fields("Built"))
  expect_false(file.exists(file.path(extracted, "foo", "Meta", "package.rds")))

  # pkgdepends notices that this "binary" is really a source package
  expect_no_error(suppressMessages(prop$install()))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "Meta", "package.rds")))
})
