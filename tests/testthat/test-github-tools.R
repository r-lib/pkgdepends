
context("github utility functions")

test_that("resolve_ref_github_deps", {

  resolve_ref_github_deps <-
    environment(parse_remote.remote_specs_github)$resolve_ref_github_deps

  deps <- data.frame(
    stringsAsFactors = FALSE,
    type = c(rep("Suggests", 4), rep("Imports", 4)),
    package = c("covr", "jsonlite", "testthat", "assertthat", "curl", "R6",
      "rlang", "uuid"),
    version = c("*", "*", "*", "*", ">= 2.8.9000", "*", "*", "*")
  )
  remotes <- c(Remotes = "\n    jeroen/curl")
  dependencies <- "Imports"

  obj <- resolve_ref_github_deps(deps, remotes, dependencies)

  exp <- tibble::tibble(
    ref = c("jeroen/curl", "R6", "rlang", "uuid"),
    type = "Imports",
    package = c("curl", "R6", "rlang", "uuid"),
    op = c(">=", "", "", ""),
    version = c("2.8.9000", "", "", "")
  )

  expect_equal(obj, exp)
})
