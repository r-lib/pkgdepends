test_that("resolve", {
  setup_fake_apps()

  conf <- current_config()

  tt <- dirname(dirname(attr(packageDescription("testthat"), "file")))
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache(),
    installed = make_installed_cache(dirname(tt))
  )

  ref <- paste0("installed::", tt)
  res <- synchronise(
    resolve_remote_installed(
      parse_pkg_refs(ref)[[1]],
      TRUE,
      conf,
      cache,
      dependencies = "Imports"
    )
  )

  unun <- function(x) {
    attr(x, "unknown_deps") <- NULL
    x
  }

  expect_equal(
    unun(as.list(res[c(
      "ref",
      "type",
      "direct",
      "status",
      "package",
      "version"
    )])),
    list(
      ref = ref,
      type = "installed",
      direct = TRUE,
      status = "OK",
      package = "testthat",
      version = as.character(utils::packageVersion("testthat"))
    )
  )

  expect_true("cli" %in% attr(res, "unknown_deps"))

  expect_false(is.null(res$extra[[1]]$repotype))
})

test_that("download", {
  setup_fake_apps()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

  tt <- dirname(dirname(attr(packageDescription("testthat"), "file")))
  ref <- paste0("installed::", tt)
  r <- pkg_plan$new(
    ref,
    library = dirname(tt),
    config = list(dependencies = FALSE, cache_dir = tmp)
  )
  expect_error(suppressMessages(r$resolve()), NA)
  expect_error(suppressMessages(r$download_resolution()), NA)
  dl <- r$get_resolution_download()
  expect_equal(dl$download_status, "Had")
})

test_that("satisfy", {
  ## Always TRUE, independently of arguments
  expect_true(satisfy_remote_installed())
})

test_that("installedok_remote_installed", {
  expect_false(installedok_remote_installed())
})

test_that("make_installed_cache", {
  # packages argument is used correctly
  cache <- make_installed_cache(.Library, "boot")
  expect_equal(cache$pkgs$package, "boot")
})

test_that("make_installed_cache edge case", {
  tmp <- withr::local_tempdir()
  cache <- make_installed_cache(tmp, "MASS")
  expect_equal(nrow(cache$pkgs), 0)
  expect_equal(nrow(cache$deps), 0)
  expect_true(ncol(cache$pkgs) >= 20) # approx
  expect_true(ncol(cache$deps) >= 5) # approx
})
