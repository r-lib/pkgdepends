
context("print")

test_that("cat_msg", {
  out <- utils::capture.output(cat_msg("foobar"))
  expect_equal(crayon::strip_style(out), "foobar")
})

test_that("format_items", {
  cases <- list(
    list(c("1", "2", "3"), "`1`, `2` and `3`"),
    list("1", "`1`"),
    list(c("1", "2"), "`1` and `2`")
  )
  for (c in cases) expect_equal(format_items(c[[1]]), c[[2]])
})
