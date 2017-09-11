
context("resolution-cran")

test_that("remotes__update_cran_cache", {

  skip_if_offline()

  dir.create(repo <- tempfile())
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  cache <- remotes_i_update_cache_cran(
    repo,
    platforms = c("source", "macos", "windows"),
    rversion = "3.4.1",
    mirror = "https://cran.rstudio.com"
  )

  expect_true(async::is_deferred(cache))

  cres <- await(cache)

  expect_true(is.list(cres))
  expect_true("_dirs" %in% names(cres))
  expect_true("_archive" %in% names(cres))
  expect_true(is.data.frame(cres$`_dirs`))
  expect_true(is.list(cres$`_archive`))
  expect_true(is.data.frame(cres$`_archive`))

  expect_true("src/contrib" %in% names(cres))
  expect_true(any(grepl("windows", names(cres))))
  expect_true(any(grepl("macosx", names(cres))))
  expect_true(any(grepl("3.4", names(cres), fixed = TRUE)))
})

test_that("make_cran_resolution", {
  res <- make_cran_resolution(
    remote = parse_remotes("ggplot2")[[1]],
    platform = "source",
    rversion = "3.4",
    data = available.packages(repos = "https://cran.rstudio.com"),
    dir = "src/contrib",
    mirror = "https://cran.rstudio.com",
    dependencies = c("Imports", "Suggests")
  )

  expect_equal(length(res$source), 2)
  expect_match(
    res$source[1],
    "^https://cran\\.rstudio\\.com/.*/ggplot2.*\\.tar\\.gz$"
  )
  expect_match(
    res$source[2],
    "^https://cran\\.rstudio\\.com/.*/Archive/ggplot2/ggplot2.*\\.tar\\.gz$"
  )

  expect_match(res$target, "^src/contrib/ggplot.*\\.tar\\.gz$")
  expect_equal(res$platform, "source")
  expect_equal(res$rversion, "3.4")
  expect_equal(res$dir, "src/contrib")
  expect_equal(res$package, "ggplot2")
  expect_silent(package_version(res$version))
  expect_true(is.character(res$deps) && length(res$deps) > 0)
  expect_equal(res$status, "OK")
})

test_that("remotes__resolve_ref_cran_current", {
  ## TODO
})
