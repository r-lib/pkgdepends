test_that("q_library_0", {
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

test_that("q_library_1", {
  expect_snapshot({
    code_query("library(pkg)", q_library_1())[["matched_captures"]]
    code_query("library('pkg')", q_library_1())[["matched_captures"]]
    code_query("require(pkg)", q_library_1())[["matched_captures"]]
    code_query("require('pkg')", q_library_1())[["matched_captures"]]
  })
})
