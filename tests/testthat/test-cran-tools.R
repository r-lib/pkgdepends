
context("cran utility functions")

test_that("type_cran_fix_cran_version", {

  packages <- format_packages_gz(
    readRDS("fixtures/resolve-cran-version-packages.rds"))
  archive <- readRDS("fixtures/resolve-cran-version-archive.rds")

  fix <- function(package, version, ge = "") {
    type_cran_fix_cran_version(package, version, ge, packages, archive)
  }

  ## Last version
  expect_equal(fix("dplyr", "last"), "current")

  ## Last version, pinned
  expect_equal(fix("dplyr", "0.7.2"), "current")

  ## Old version, pinned
  expect_equal(fix("dplyr", "0.3.0.2"), "0.3.0.2")

  ## Last version of archived package
  expect_equal(fix("igraph0", "last"), "0.5.7")

  ## Old version of archive package, pinned
  expect_equal(fix("igraph0", "0.5.5-3"), "0.5.5-3")

  ## Recent versions of active package
  expect_equal(
    fix("dplyr", ge = ">=", "0.7.0"),
    c("0.7.0", "0.7.1", "current")
  )
  expect_equal(fix("dplyr", ge = ">=", "0.7.2"), "current")

  ## Recent versions of archived package
  expect_equal(
    fix("igraph0", ge = ">=", "0.5.6-1"),
    c("0.5.6-1", "0.5.6-2", "0.5.7")
  )
  expect_equal(fix("igraph0", ge = ">=", "0.5.7"), "0.5.7")
})

test_that("parse_deps", {

  expect_equal(
    parse_deps(character(), character()),
    list()
  )

  expect_equal(
    parse_deps("", "Imports"),
    list(tibble::tibble(
      type = character(), package = character(), op = character(),
      version = character()
    ))
  )

  expect_equal(
    parse_deps("foobar", "Imports"),
    list(tibble::tibble(
      type = "Imports",
      package = "foobar",
      op = "",
      version = ""
    ))
  )

  expect_equal(
    parse_deps("foobar (>= 1.0-5)", "Imports"),
    list(tibble::tibble(
      type = "Imports",
      package = "foobar",
      op = ">=",
      version = "1.0-5"
    ))
  )

  expect_equal(
    parse_deps("foobar\n (>=\n 1.0-5), foobar2", "Imports"),
    list(tibble::tibble(
      type = rep("Imports", 2),
      package = c("foobar", "foobar2"),
      op = c(">=", ""),
      version = c("1.0-5", "")
    ))
  )
})

test_that("type_cran_get_package_deps_url", {

  skip_if_offline()

  url <- "https://cran.rstudio.com/src/contrib/Archive/dplyr/dplyr_0.2.tar.gz"
  dir.create(dir <- tempfile())
  target <- file.path(dir, basename(url))
  obj <- async::synchronise(type_cran_get_package_deps_url(
    url = url,
    target = target,
    dependencies = c("Imports", "LinkingTo")
  ))

  exp <- tibble::tibble(
    ref = c("assertthat", "Rcpp", "magrittr", "Lahman", "hflights",
      "Rcpp", "BH"),
    type = c(rep("Imports", 5), rep("LinkingTo", 2)),
    package = ref,
    op = c(rep("", 5), rep(">=", 2)),
    version = c(rep("", 5), "0.11.1", "1.51.0-2")
  )

  expect_equal(obj, exp)
})
