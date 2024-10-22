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
