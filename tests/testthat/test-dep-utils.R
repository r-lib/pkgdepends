
context("dependency utilities")

test_that("resolve_ref_deps", {

  deps <- data.frame(
    stringsAsFactors = FALSE,
    type = c(rep("Suggests", 4), rep("Imports", 5)),
    package = c("covr", "jsonlite", "testthat", "assertthat", "curl", "R6",
      "rlang", "uuid", "bar"),
    version = c("*", "*", "*", "*", ">= 2.8.9000", "*", "*", "*", "*")
  )
  remotes <- c(Remotes = "\n    jeroen/curl,\n  foo/bar")
  dependencies <- "Imports"

  obj <- resolve_ref_deps(deps, remotes, dependencies)

  exp <- tibble::tibble(
    ref = c("jeroen/curl", "R6", "rlang", "uuid", "foo/bar"),
    type = "Imports",
    package = c("curl", "R6", "rlang", "uuid", "bar"),
    op = c(">=", "", "", "", ""),
    version = c("2.8.9000", "", "", "", "")
  )

  expect_equal(obj, exp)
})

test_that("get_cran_deps", {

  packages <- readRDS("fixtures/resolve-cran-version-packages.rds")

  deps <- c("Imports", "LinkingTo", "Depends")

  expected <- tibble::tibble(
    ref = c("assertthat", "bindrcpp", "glue", "magrittr",
      "pkgconfig", "rlang", "R6", "Rcpp", "tibble", "Rcpp", "BH",
      "bindrcpp", "plogr"),
    type = c(rep("Imports", 9), rep("LinkingTo", 4)),
    package = ref,
    op = c("", ">=", ">=", "", "", ">=", "", ">=", ">=", ">=", ">=",
      "", ""),
    version = c("", "0.2", "1.1.0", "", "", "0.1", "", "0.12.6",
      "1.3.1", "0.12.0", "1.58.0-1", "", "")
  )
  
  expect_equal(get_cran_deps("dplyr", "0.7.2", packages, deps), expected)
  expect_equal(get_cran_deps("dplyr", "", packages, deps), expected)
})
