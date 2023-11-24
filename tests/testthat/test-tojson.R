test_that("write_str vectors", {
  # empty vectors
  expect_snapshot({
    tojson$write_str(NULL)
    tojson$write_str(list())
    tojson$write_str(integer())
    tojson$write_str(double())
    tojson$write_str(character())
    tojson$write_str(logical())
  })

  # cannot convert some atomic vectors
  expect_snapshot(error = TRUE, {
    tojson$write_str(complex())
    tojson$write_str(raw())
  })

  # scalars
  expect_snapshot({
    tojson$write_str(list(1))
    tojson$write_str(1L)
    tojson$write_str(1.0)
    tojson$write_str("foo")
    tojson$write_str(TRUE)
  })

  # vectors
  expect_snapshot({
    tojson$write_str(list(1, 2))
    tojson$write_str(1:2)
    tojson$write_str(c(1.0, 2.0))
    tojson$write_str(c("foo", "bar"))
    tojson$write_str(c(TRUE, FALSE))
  })

  # NA in vectors
  expect_snapshot({
    tojson$write_str(NA_integer_)
    tojson$write_str(NA_real_)
    tojson$write_str(NA_character_)
    tojson$write_str(NA)
  })

  expect_snapshot({
    tojson$write_str(c(1L, NA_integer_))
    tojson$write_str(c(1.0, NA_real_))
    tojson$write_str(c("foo", NA_character_))
    tojson$write_str(c(TRUE, NA))
  })

  # names are ignored
  expect_snapshot({
    tojson$write_str(c(a = 1L, b = 2L))
    tojson$write_str(c(a = 1.0, b = 2.0))
    tojson$write_str(c(a = "foo", b = "bar"))
    tojson$write_str(c(a = TRUE, b = FALSE))
  })
})

test_that("write_str character encoding and escaping", {
  utf8 <- "G\u00e1bor"
  expect_snapshot({
    cat(tojson$write_str("foo\"\\bar"))
    charToRaw(tojson$write_str(iconv(utf8, "UTF-8", "latin1")))
  })
})

test_that("lists", {
  # embedded lists
  expect_snapshot({
    cat(tojson$write_str(list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))))
  })

  # named lists
  expect_snapshot({
    cat(tojson$write_str(list(a = 1, b = 2)))
  })

  # nested named lists
  expect_snapshot({
    cat(tojson$write_str(list(
        a = list(a1 = 1, a2 = 2),
        b = list(b1 = 3, b2 = 4)
    )))
  })

  # fill in names
  expect_snapshot({
    cat(tojson$write_str(list(
        a = list(1, a2 = 2),
        list(b1 = 3, 4)
    )))
  })
})

test_that("write_file", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  tojson$write_file(mtcars, tmp)
  lns <- tojson$write_lines(mtcars)
  expect_equal(readLines(tmp), lns)
})

test_that("auto-unbox", {
  # scalars
  expect_snapshot({
    tojson$write_str(list(1), opts = list(auto_unbox = TRUE))
    tojson$write_str(1L, opts = list(auto_unbox = TRUE))
    tojson$write_str(1.0, opts = list(auto_unbox = TRUE))
    tojson$write_str("foo", opts = list(auto_unbox = TRUE))
    tojson$write_str(TRUE, opts = list(auto_unbox = TRUE))
  })

  # NA in vectors
  expect_snapshot({
    tojson$write_str(NA_integer_, opts = list(auto_unbox = TRUE))
    tojson$write_str(NA_real_, opts = list(auto_unbox = TRUE))
    tojson$write_str(NA_character_, opts = list(auto_unbox = TRUE))
    tojson$write_str(NA, opts = list(auto_unbox = TRUE))
  })

  # embedded lists
  expect_snapshot({
    cat(tojson$write_str(
      list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9)),
      opts = list(auto_unbox = TRUE)
    ))
  })

  # named lists
  expect_snapshot({
    cat(tojson$write_str(
      list(a = 1, b = 2),
      opts = list(auto_unbox = TRUE)
    ))
  })

  # nested named lists
  expect_snapshot({
    cat(tojson$write_str(
      list(
        a = list(a1 = 1, a2 = 2),
        b = list(b1 = 3, b2 = 4)
      ),
      opts = list(auto_unbox = TRUE)
    ))
  })

  # mixing scalars and vectors
  expect_snapshot({
    cat(tojson$write_str(
      list(list(1, 2:3, 4), list(4:5, 6, 7:8), list(9:10, 11, 12)),
      opts = list(auto_unbox = TRUE)
    ))
  })

  expect_snapshot({
    cat(tojson$write_str(
      list(a = 1:3, b = 4),
      opts = list(auto_unbox = TRUE)
    ))
  })

  expect_snapshot({
    cat(tojson$write_str(
      list(
        a = list(a1 = 1, a2 = 2:3),
        b = list(b1 = 4:6, b2 = 7)
      ),
      opts = list(auto_unbox = TRUE)
    ))
  })
})

test_that("unbox", {
  # scalars
  expect_snapshot({
    tojson$write_str(list(tojson$unbox(1)))
    tojson$write_str(tojson$unbox(1L))
    tojson$write_str(tojson$unbox(1.0))
    tojson$write_str(tojson$unbox("foo"))
    tojson$write_str(tojson$unbox(TRUE))
  })

  # NA in vectors
  expect_snapshot({
    tojson$write_str(tojson$unbox(NA_integer_))
    tojson$write_str(tojson$unbox(NA_real_))
    tojson$write_str(tojson$unbox(NA_character_))
    tojson$write_str(tojson$unbox(NA))
  })

  # embedded lists
  expect_snapshot({
    cat(tojson$write_str(
      list(
        list(1, tojson$unbox(2), 3),
        list(tojson$unbox(4), 5, 6),
        list(7, 8, 9)
      )
    ))
  })

  # named lists
  expect_snapshot({
    cat(tojson$write_str(
      list(a = 1, b = tojson$unbox(2))
    ))
  })

  # nested named lists
  expect_snapshot({
    cat(tojson$write_str(
      list(
        a = list(a1 = tojson$unbox(1), a2 = 2),
        b = list(b1 = 3, b2 = tojson$unbox(4))
      )
    ))
  })

  expect_snapshot(error = TRUE, {
    tojson$unbox(list())
    tojson$unbox(1:2)
    tojson$unbox(double(2))
    tojson$unbox(character(2))
    tojson$unbox(logical(2))
  })
})

test_that("pretty", {
  # vectors
  expect_snapshot({
    tojson$write_str(1:5, opts = list(pretty = TRUE))
    tojson$write_str(1:5/2, opts = list(pretty = TRUE))
    tojson$write_str(letters[1:5], opts = list(pretty = TRUE))
    tojson$write_str(1:5 %% 2 == 0, opts = list(pretty = TRUE))
  })

  # lists
  expect_snapshot({
    cat(tojson$write_str(
      list(
        list(1, tojson$unbox(2), 3),
        list(tojson$unbox(4), 5, 6),
        list(7, 8, 9)
      ),
      opts = list(pretty = TRUE)
    ))
  })

  # named lists
  expect_snapshot({
    cat(tojson$write_str(
      list(a = 1, b = tojson$unbox(2)),
      opts = list(pretty = TRUE)
    ))
  })

  # nested named lists
  expect_snapshot({
    cat(tojson$write_str(
      list(
        a = list(a1 = tojson$unbox(1), a2 = 2),
        b = list(b1 = 3, b2 = tojson$unbox(4))
      ),
      opts = list(pretty = TRUE)
    ))
  })
})

test_that("data frames", {
  # easy
  df <- mtcars[1:3, 1:3]
  expect_snapshot({
    tojson$write_str(df)
    cat(tojson$write_str(df, opts = list(pretty = TRUE)))
  })

  # NAs
  df[1, 1] <- df[2, 3] <- NA
  expect_snapshot({
    tojson$write_str(df)
    cat(tojson$write_str(df, opts = list(pretty = TRUE)))
  })

  # list columns
  df$list <- I(list(1:2, 5, letters[1:3]))
  expect_snapshot({
    tojson$write_str(df)
    cat(tojson$write_str(df, opts = list(pretty = TRUE)))
  })
})
