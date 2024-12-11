test_that("get_dep_type_from_path", {
  expect_snapshot({
    get_dep_type_from_path(c(
      "R/foo.R",
      "man/roxygen/meta.R",
      "tests/test-1.R",
      "test/test-2.R"
    ))
  })
})
