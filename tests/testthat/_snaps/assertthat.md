# failure in predicate

    Code
      assert_that(assert())
    Error <assertError>
      ! isTRUE(x = FALSE) is not true

# assert_error

    Code
      fn()
    Output
      <extraClass/assertError/rlib_error_3_0/rlib_error/error>
      Error in `fn()`:
      ! FALSE is not TRUE

# assertion returns invalid value

    Code
      assert_that(2 * 2)
    Error <rlib_error_3_0>
      ! `assert_that()`: assertion must return a logical value.
      i it returned a number instead.

---

    Code
      assert_that(c(TRUE, FALSE))
    Error <rlib_error_3_0>
      ! `assert_that()`: assertion must return a scalar.
      i it returned a vector of length 2.

---

    Code
      assert_that(NA)
    Error <rlib_error_3_0>
      ! `assert_that()`: assertion must not return `NA`.

# default messages

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(x == 1)
      > fn(2)
      [1m[33mError[39m[22m in `fn(2)`:
      [33m![39m [1m[22m`x` must equal `1`.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(x < 1)
      > fn(2)
      [1m[33mError[39m[22m in `fn(2)`:
      [33m![39m [1m[22m`x` must be less than `1`.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(x > 2)
      > fn(1)
      [1m[33mError[39m[22m in `fn(1)`:
      [33m![39m [1m[22m`x` must be greater than `2`.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(x >= 2)
      > fn(1)
      [1m[33mError[39m[22m in `fn(1)`:
      [33m![39m [1m[22m`x` must be greater than or equal to `2`.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(x <= 1)
      > fn(2)
      [1m[33mError[39m[22m in `fn(2)`:
      [33m![39m [1m[22m`x` must be less than or equal to `1`.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(x != 1)
      > fn(2)

---

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is.atomic(x))
      > fn(mtcars)
      [1m[33mError[39m[22m in `fn(mtcars)`:
      [33m![39m [1m[22m`x` must be an atomic vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.character(x))
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a character vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.complex(x))
      > fn(1:5)
      [1m[33mError[39m[22m in `fn(1:5)`:
      [33m![39m [1m[22m`x` must be a complex vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.double(x))
      > fn(letters)
      [1m[33mError[39m[22m in `fn(letters)`:
      [33m![39m [1m[22m`x` must be a numeric vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.integer(x))
      > fn(letters)
      [1m[33mError[39m[22m in `fn(letters)`:
      [33m![39m [1m[22m`x` must be an integer vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.numeric(x))
      > fn(letters)
      [1m[33mError[39m[22m in `fn(letters)`:
      [33m![39m [1m[22m`x` must be a numeric or integer vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.raw(x))
      > fn(1:10)
      [1m[33mError[39m[22m in `fn(1:10)`:
      [33m![39m [1m[22m`x` must be a raw vector.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.vector(x))
      > fn(mtcars)
      [1m[33mError[39m[22m in `fn(mtcars)`:
      [33m![39m [1m[22m`x` must be an atomic vector without attributes.
      [90mType .Last.error to see the more details.[39m

---

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is.factor(x))
      > fn(letters)
      [1m[33mError[39m[22m in `fn(letters)`:
      [33m![39m [1m[22m`x` must be a factor.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.ordered(x))
      > fn(factor(letters))
      [1m[33mError[39m[22m in `fn(factor(letters))`:
      [33m![39m [1m[22m`x` must be an ordered factor.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.array(x))
      > fn(factor(letters))
      [1m[33mError[39m[22m in `fn(factor(letters))`:
      [33m![39m [1m[22m`x` must be an array.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.ordered(x))
      > fn(factor(letters))
      [1m[33mError[39m[22m in `fn(factor(letters))`:
      [33m![39m [1m[22m`x` must be an ordered factor.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.array(x))
      > fn(letters)
      [1m[33mError[39m[22m in `fn(letters)`:
      [33m![39m [1m[22m`x` must be an array.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.data.frame(x))
      > fn(1:10)
      [1m[33mError[39m[22m in `fn(1:10)`:
      [33m![39m [1m[22m`x` must be a data frame.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.list(x))
      > fn("foobar")
      [1m[33mError[39m[22m in `fn("foobar")`:
      [33m![39m [1m[22m`x` must be a list.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.matrix(x))
      > fn(letters)
      [1m[33mError[39m[22m in `fn(letters)`:
      [33m![39m [1m[22m`x` must be a matrix.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.null(x))
      > fn("not")
      [1m[33mError[39m[22m in `fn("not")`:
      [33m![39m [1m[22m`x` must be `NULL`.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.environment(x))
      > fn(list())
      [1m[33mError[39m[22m in `fn(list())`:
      [33m![39m [1m[22m`x` must be an environment.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.function(x))
      > fn("clearly noy")
      [1m[33mError[39m[22m in `fn("clearly noy")`:
      [33m![39m [1m[22m`x` must be a function.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.promitive(x))
      > fn(asesrt_that)
      Error in is.promitive(x) : could not find function "is.promitive"
      > fn <- function(x) assert_that(is.call(x))
      > fn("not")
      [1m[33mError[39m[22m in `fn("not")`:
      [33m![39m [1m[22m`x` must be a quoted call.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.expression(x))
      > fn(mtcars)
      [1m[33mError[39m[22m in `fn(mtcars)`:
      [33m![39m [1m[22m`x` must be an expression object.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.name(x))
      > fn("not really")
      [1m[33mError[39m[22m in `fn("not really")`:
      [33m![39m [1m[22m`x` must be a name.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.pairlist(x))
      > fn(1:10)
      [1m[33mError[39m[22m in `fn(1:10)`:
      [33m![39m [1m[22m`x` must be a pairlist.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.recursive(x))
      > fn(1)
      [1m[33mError[39m[22m in `fn(1)`:
      [33m![39m [1m[22m`x` must be a recursive object.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(is.symbol(x))
      > fn("almost")
      [1m[33mError[39m[22m in `fn("almost")`:
      [33m![39m [1m[22m`x` must be a name.
      [90mType .Last.error to see the more details.[39m

---

    Code
      r_process()
    Output
      > fn <- function(x, y) assert_that(x && y)
      > fn(is.integer(1L), is.integer("a"))
      [1m[33mError[39m[22m in `fn(is.integer(1L), is.integer("a"))`:
      [33m![39m [1m[22m`x` and `y` must both be true.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x, y) assert_that(x || y)
      > fn(is.integer("b"), is.integer("a"))
      [1m[33mError[39m[22m in `fn(is.integer("b"), is.integer("a"))`:
      [33m![39m [1m[22mOne of `x` and `y` must be true.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(any(x))
      > fn(rep(FALSE, 10))
      [1m[33mError[39m[22m in `fn(rep(FALSE, 10))`:
      [33m![39m [1m[22mAt least one of `x` must be true.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(all(x))
      > fn(c(FALSE, TRUE, TRUE))
      [1m[33mError[39m[22m in `fn(c(FALSE, TRUE, TRUE))`:
      [33m![39m [1m[22mAll of `x` must be true.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x) assert_that(file.exists(x))
      > fn("/file7e0fe1739e0")
      [1m[33mError[39m[22m in `fn("/file7e0fe1739e0")`:
      [33m![39m [1m[22mPath `x` must exist.
      [90mType .Last.error to see the more details.[39m
      > fn <- function(x, y) assert_that(identical(x, y))
      > fn(1, 1L)
      [1m[33mError[39m[22m in `fn(1, 1L)`:
      [33m![39m [1m[22m`x` must be identical to `y`.
      [90mType .Last.error to see the more details.[39m

# long call is truncated

    Code
      fail_default(call("==", strrep("-", 100), strrep("x", 100)))
    Output
      [1] "\"----------------------------------------------------------------------------------------------------\" == ... is not true"

# has_attr

    Code
      assert_that(has_attr(1L, "foobar"))
    Error <assertError>
      ! `1L` must have attribute `foobar`.

---

    Code
      assert_that(1L %has_attr% "foobar")
    Error <assertError>
      ! `1L` must have attribute `foobar`.

# custom message

    Code
      fn(1:10)
    Error <assertError>
      ! `name` must be a name (character scalar).
      i It is an integer vector.

