
test_that("draw_solution_tree", {

  skip_if_offline()
  skip_on_cran()

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  r <- pkg_plan$new(c("pkgconfig", "igraph"), library = lib)
  r$resolve()
  r$solve()

  out <- r$draw_solution_tree()
  expect_s3_class(out, "tree")
  expect_true(is.character(out))
})
