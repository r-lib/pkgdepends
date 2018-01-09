
context("resolution-cran")

test_that("remotes__update_cran_cache", {

  skip_if_offline()

  dir.create(cache_dir <- tempfile())
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  afun <- async(function() {
    cache <- type_cran_update_cache(
      cache_dir,
      platforms = c("source", "macos", "windows"),
      rversion = "3.4.1",
      mirror = "https://cran.rstudio.com"
    )

    expect_true(async::is_deferred(cache))

    cache
  })

  cres <- synchronise(afun())

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

test_that("type_cran_make_resolution", {
  av_pkg <- withr::with_options(
    list(repos = c(CRAN = "https://cran.rstudio.com")),
    available.packages()
  )
  res <- type_cran_make_resolution(
    remote = parse_remotes("ggplot2")[[1]],
    platform = "source",
    rversion = "3.4",
    data = format_packages_gz(av_pkg),
    dir = "src/contrib",
    mirror = "https://cran.rstudio.com",
    dependencies = c("Imports", "Suggests")
  )[[1]]

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
  expect_true(is.data.frame(res$deps) && nrow(res$deps) > 0)
  expect_equal(res$status, "OK")
})

test_that("remotes__resolve_ref_cran_current", {
  ## TODO
})
