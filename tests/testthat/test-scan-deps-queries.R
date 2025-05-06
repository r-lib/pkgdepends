test_that("q_library_0", {
  local_reproducible_output(width = 500)
  do <- function(code) {
    code_query(code, q_library_0())[["matched_captures"]]
  }
  expect_snapshot({
    do("library(pkg, lib.loc = path)")
    do("library('pkg', lib.loc = path)")
    do("library(lib.loc = path, pkg)")
    do("require(lib.loc = path, character.only = TRUE, 'pkg')")
    do("library(foo, require(bar))")
  })
})

test_that("q_module_import", {
  local_reproducible_output(width = 500)
  do <- function(path) {
    apath <- test_path(path)
    code_query(readLines(apath), q_module_import())[["matched_captures"]]
  }
  expect_snapshot({
    do("fixtures/scan/modules.R")
    do("fixtures/scan/modules-empty.R")
  })
})

test_that("q_colon", {
  local_reproducible_output(width = 500)
  do <- function(code) {
    code_query(code, q_colon())[["matched_captures"]]
  }
  expect_snapshot({
    do("x <- foo::bar()")
    do("1 + 2 + foo:::bar")
  })
})

test_that("q_methods", {
  local_reproducible_output(width = 500)
  do <- function(code) {
    code_query(code, q_methods())[["matched_captures"]]
  }
  expect_snapshot({
    do("setClass('myclass')")
    do("setGeneric('props', function(object) attributes(object))")
  })
})

test_that("q_junit_reporter", {
  local_reproducible_output(width = 500)
  do <- function(code) {
    code_query(code, q_junit_reporter())[["matched_captures"]]
  }
  expect_snapshot({
    do("JunitReporter$new()")
    do("testthat::JunitReporter$new()")
  })
})

test_that("q_knitr_dev", {
  local_reproducible_output(width = 500)
  do <- function(code) {
    code_query(code, q_knitr_dev())[["matched_captures"]]
  }
  expect_snapshot({
    do("opts_chunk$set()")
    do("knitr::opts_chunk$set()")
  })
})

test_that("renv_dependencies_database", {
  expect_snapshot({
    renv_dependencies_database()
  })
})

test_that("q_database", {
  local_reproducible_output(width = 500)
  do <- function(code) {
    code_query(code, q_database())[["matched_captures"]]
  }
  expect_snapshot({
    do("geom_hex()")
    do("ggplot2::geom_hex()")
    do("JunitReporter")
    do("testthat::JunitReporter")
  })
})

test_that("q_database #2", {
  fake(q_database, "renv_dependencies_database", NULL)
  expect_null(q_database())
})

test_that("q_database #3", {
  local_reproducible_output(width = 500)
  withr::local_options(
    renv.dependencies.database = list(foopkg = list(foofun = "foodep"))
  )
  do <- function(code) {
    code_query(code, q_database())[["matched_captures"]]
  }
  expect_snapshot({
    do("geom_hex()")
    do("foopkg::foofun()")
    do("foofun")
  })
})

test_that("q_deps", {
  fake(q_deps, "q_library_0", 1)
  fake(q_deps, "q_colon", 2)
  fake(q_deps, "q_methods", 3)
  fake(q_deps, "q_junit_reporter", 4)
  fake(q_deps, "q_knitr_dev", 5)
  fake(q_deps, "q_database", 6)
  expect_snapshot(
    q_deps()
  )
})

test_that("q_deps_rmd", {
  local_reproducible_output(width = 500)

  expect_snapshot({
    code_query(
      readLines(test_path("fixtures/scan/chunk-errors.Rmd")),
      query = q_deps_rmd(),
      language = "markdown"
    )[["matched_captures"]]
  })

  expect_snapshot({
    code_query(
      readLines(test_path("fixtures/scan/inline-chunks.Rmd")),
      query = q_deps_rmd(),
      language = "markdown"
    )[["matched_captures"]]
  })
})

test_that("q_deps_rmd_inline", {
  local_reproducible_output(width = 500)

  code <- code_query(
    readLines(test_path("fixtures/scan/inline-chunks.Rmd")),
    query = q_deps_rmd(),
    language = "markdown"
  )[["matched_captures"]]

  expect_snapshot({
    code_query(
      readLines(test_path("fixtures/scan/inline-chunks.Rmd")),
      query = q_deps_rmd_inline(),
      language = "markdown-inline",
      ranges = code[, range_cols]
    )[["matched_captures"]]
  })
})

test_that("q_deps_yaml_header", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    print(
      n = Inf,
      code_query(
        readLines(test_path("fixtures/scan/header-shiny.Rmd")),
        query = q_deps_yaml_header(),
        language = "yaml"
      )[["matched_captures"]]
    )
    print(
      n = Inf,
      code_query(
        readLines(test_path("fixtures/scan/header-shiny2.Rmd")),
        query = q_deps_yaml_header(),
        language = "yaml"
      )[["matched_captures"]]
    )
  })
})
