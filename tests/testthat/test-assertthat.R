
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
  asciicast::expect_snapshot_r_process(
    transform = function(x) {
      transform_no_srcref(transform_no_links(transform_show_cursor(x)))
    },
    fn <- function(x) assert_that(x == 1),
    fn(2),

    fn <- function(x) assert_that(x < 1),
    fn(2),

    fn <- function(x) assert_that(x > 2),
    fn(1),

    fn <- function(x) assert_that(x >= 2),
    fn(1),

    fn <- function(x) assert_that(x <= 1),
    fn(2),

    fn <- function(x) assert_that(x != 1),
    fn(2)
  )

  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is.atomic(x)),
    fn(mtcars),

    fn <- function(x) assert_that(is.character(x)),
    fn(1:2),

    fn <- function(x) assert_that(is.complex(x)),
    fn(1:5),

    fn <- function(x) assert_that(is.double(x)),
    fn(letters),

    fn <- function(x) assert_that(is.integer(x)),
    fn(letters),

    fn <- function(x) assert_that(is.numeric(x)),
    fn(letters),

    fn <- function(x) assert_that(is.raw(x)),
    fn(1:10),

    fn <- function(x) assert_that(is.vector(x)),
    fn(mtcars)
  )

  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is.factor(x)),
    fn(letters),

    fn <- function(x) assert_that(is.ordered(x)),
    fn(factor(letters)),

    fn <- function(x) assert_that(is.array(x)),
    fn(factor(letters)),

    fn <- function(x) assert_that(is.ordered(x)),
    fn(factor(letters)),

    fn <- function(x) assert_that(is.array(x)),
    fn(letters),

    fn <- function(x) assert_that(is.data.frame(x)),
    fn(1:10),

    fn <- function(x) assert_that(is.list(x)),
    fn("foobar"),

    fn <- function(x) assert_that(is.matrix(x)),
    fn(letters),

    fn <- function(x) assert_that(is.null(x)),
    fn("not"),

    fn <- function(x) assert_that(is.environment(x)),
    fn(list()),

    fn <- function(x) assert_that(is.function(x)),
    fn("clearly noy"),

    fn <- function(x) assert_that(is.promitive(x)),
    fn(asesrt_that),

    fn <- function(x) assert_that(is.call(x)),
    fn("not"),

    fn <- function(x) assert_that(is.expression(x)),
    fn(mtcars),

    fn <- function(x) assert_that(is.name(x)),
    fn("not really"),

    fn <- function(x) assert_that(is.pairlist(x)),
    fn(1:10),

    fn <- function(x) assert_that(is.recursive(x)),
    fn(1),

    fn <- function(x) assert_that(is.symbol(x)),
    fn("almost"),

  )

  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x, y) assert_that(x && y),
    fn(is.integer(1L), is.integer("a")),

    fn <- function(x, y) assert_that(x || y),
    fn(is.integer("b"), is.integer("a")),

    fn <- function(x) assert_that(any(x)),
    fn(rep(FALSE, 10)),

    fn <- function(x) assert_that(all(x)),
    fn(c(FALSE, TRUE, TRUE)),

    fn <- function(x) assert_that(file.exists(x)),
    fn("/file7e0fe1739e0"),

    fn <- function(x, y) assert_that(identical(x, y)),
    fn(1, 1L)
  )
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

test_that("custom message", {
  fn <- function(name) {
    assert_that(
      is_string(name),
      msg = c(
        "{.arg {(.arg)}} must be a name (character scalar).",
        i = "It is {.type {name}}."
      )
    )
  }
  expect_snapshot(
    error = TRUE,
    fn(1:10)
  )
})
