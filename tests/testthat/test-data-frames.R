
context("data frames")

example_df <- function() {
  data.frame(
    stringsAsFactors = FALSE,
    a = 1:5,
    b = letters[1:5],
    c = 5:1,
    d = LETTERS[5:1],
    e = c(1,1,1,2,2)
  )
}

test_that("find rows", {
  df <- example_df()
  expect_equal(find_in_data_frame(df, a = 1), 1L)
  expect_equal(find_in_data_frame(df), 1:5)
  expect_equal(find_in_data_frame(df, e = 1), 1:3)
  expect_equal(find_in_data_frame(df, a = 2, e = 1), 2L)
  expect_equal(find_in_data_frame(df, a = 4, e = 1), integer())
})

test_that("append rows", {
  df <- example_df()
  expect_equal(
    as.list(append_to_data_frame(df = df, a = 9, b = "x", c = 9,
                                 d = "A", e = 90)[6,]),
    list(a = 9, b = "x", c = 9, d = "A", e = 90)
  )
  expect_equal(
    as.list(append_to_data_frame(df = df, b = "x", c = 9,
                                 d = "A", e = 90)[6,]),
    list(a = NA_integer_, b = "x", c = 9, d = "A", e = 90)
  )
  expect_equal(
    as.list(append_to_data_frame(df = df, b = "x")[6,]),
    list(a = NA_integer_, b = "x", c = NA_integer_, d = NA_character_,
         e = NA_real_)
  )
  expect_equal(
    as.list(append_to_data_frame(df = df)[6,]),
    list(a = NA_integer_, b = NA_character_, c = NA_integer_,
         d = NA_character_, e = NA_real_)
  )
})

test_that("append to empty", {
  df <- example_df()[numeric(), ]
  df2 <- append_to_data_frame(df = df, a = 10, e = 20, d = "X")
  expect_equal(names(df), names(df2))
})

test_that("delete rows", {
  df <- example_df()
  expect_equal(delete_from_data_frame(df, a = 1), df[-1,])
  expect_equal(delete_from_data_frame(df, e = 1), df[-(1:3),])
  expect_equal(delete_from_data_frame(df), df[numeric(),])
  expect_equal(delete_from_data_frame(df, a = -1), df)
  expect_equal(delete_from_data_frame(df, a = 2, e = 1), df[-2,])
})
