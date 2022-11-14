
test_that("basic", {
  expect_true(assert_that())
  expect_true(assert_that(TRUE))
  expect_true(assert_that(TRUE, TRUE))
  expect_true(assert_that(TRUE, TRUE, TRUE))

  expect_error(assert_that(FALSE), class = "assertError")
  expect_error(assert_that(TRUE, FALSE), class = "assertError")
})

test_that("failure in predicate", {
  assert <- function() assert_that(isTRUE(FALSE))
  expect_snapshot(
    error = TRUE,
    assert_that(assert())
  )
})

test_that("assert_error", {
  fn <- function() {
    assert_error(
      quote(isTRUE(FALSE)),
      result = FALSE,
      msg = "FALSE is not TRUE",
      .data = list(extra = "info"),
      .class = "extraClass",
      call. = sys.call()
    )
  }
  expect_snapshot(fn())
})

test_that("assertion returns invalid value", {
  expect_snapshot(
    error = TRUE,
    assert_that(2 * 2)
  )
  expect_snapshot(
    error = TRUE,
    assert_that(c(TRUE, FALSE))
  )
  expect_snapshot(
    error = TRUE,
    assert_that(NA)
  )
})

test_that("default messages", {
  expect_snapshot(error = TRUE, assert_that(1 == 2))
  expect_snapshot(error = TRUE, assert_that(2 < 1))
  expect_snapshot(error = TRUE, assert_that(1 > 2))
  expect_snapshot(error = TRUE, assert_that(1 >= 2))
  expect_snapshot(error = TRUE, assert_that(2 <= 1))
  expect_snapshot(error = TRUE, assert_that(1 != 1))

  expect_snapshot(error = TRUE, assert_that(is.atomic(mtcars)))
  expect_snapshot(error = TRUE, assert_that(is.character(1:2)))
  expect_snapshot(error = TRUE, assert_that(is.complex(1:5)))
  expect_snapshot(error = TRUE, assert_that(is.double(letters)))
  expect_snapshot(error = TRUE, assert_that(is.integer(letters)))
  expect_snapshot(error = TRUE, assert_that(is.numeric(letters)))
  expect_snapshot(error = TRUE, assert_that(is.raw(1:10)))
  expect_snapshot(error = TRUE, assert_that(is.vector(mtcars)))

  expect_snapshot(error = TRUE, assert_that(is.factor(letters)))
  expect_snapshot(error = TRUE, assert_that(is.ordered(factor(letters))))

  expect_snapshot(error = TRUE, assert_that(is.array(letters)))
  expect_snapshot(error = TRUE, assert_that(is.data.frame(1:10)))
  expect_snapshot(error = TRUE, assert_that(is.list("foobar")))
  expect_snapshot(error = TRUE, assert_that(is.matrix(letters)))
  expect_snapshot(error = TRUE, assert_that(is.null("not")))

  expect_snapshot(error = TRUE, assert_that(is.environment(list())))
  expect_snapshot(error = TRUE, assert_that(is.function("clearly noy")))
  expect_snapshot(error = TRUE, assert_that(is.primitive(assert_that)))

  expect_snapshot(error = TRUE, assert_that(is.call("not")))
  expect_snapshot(error = TRUE, assert_that(is.expression(mtcars)))
  expect_snapshot(error = TRUE, assert_that(is.name("not really")))
  expect_snapshot(error = TRUE, assert_that(is.pairlist(1:10)))
  expect_snapshot(error = TRUE, assert_that(is.recursive(1)))
  expect_snapshot(error = TRUE, assert_that(is.symbol("almost")))

  expect_snapshot(error = TRUE, assert_that(inherits(letters, "myclass")))

  expect_snapshot(error = TRUE, assert_that(TRUE && FALSE))

  expect_snapshot(error = TRUE, assert_that(FALSE || FALSE))

  expect_snapshot(error = TRUE, assert_that(any(rep(FALSE, 10))))
  expect_snapshot(error = TRUE, assert_that(all(c(FALSE, TRUE, TRUE))))

  expect_snapshot(error = TRUE, assert_that(file.exists("/file7e0fe1739e0")))
  expect_snapshot(error = TRUE, assert_that(identical(1, 1L)))  
})

test_that("long call is truncated", {
  expect_snapshot(
    fail_default(call("==", strrep("-", 100), strrep("x", 100)))
  )
})

test_that("has_attr", {
  expect_true(
    has_attr(mtcars, "class"))
  expect_snapshot(
    error = TRUE,
    assert_that(has_attr(1L, "foobar"))
  )

  expect_true(mtcars %has_attr% "class")
  expect_snapshot(
    error = TRUE,
    assert_that(1L %has_attr% "foobar")
  )
})
