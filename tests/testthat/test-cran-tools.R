
context("cran utility functions")

test_that("fix_cran_version", {

  fix_cran_version <-
    environment(parse_remote.remote_specs_cran)$fix_cran_version

  packages <- readRDS("fixtures/resolve-cran-version-packages.rds")
  archive <- readRDS("fixtures/resolve-cran-version-archive.rds")

  fix <- function(package, version, ge = "") {
    fix_cran_version(package, version, ge, packages, archive)
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
