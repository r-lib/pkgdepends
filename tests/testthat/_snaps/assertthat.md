# failure in predicate

    Code
      assert_that(assert())
    Error <assertError>
      ! isTRUE(x = FALSE) is not TRUE

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
      assert_that(1 == 2)
    Error <assertError>
      ! 1 not equal to 2

---

    Code
      assert_that(2 < 1)
    Error <assertError>
      ! 2 not less than 1

---

    Code
      assert_that(1 > 2)
    Error <assertError>
      ! 1 not greater than 2

---

    Code
      assert_that(1 >= 2)
    Error <assertError>
      ! 1 not greater than or equal to 2

---

    Code
      assert_that(2 <= 1)
    Error <assertError>
      ! 2 not less than or equal to 1

---

    Code
      assert_that(1 != 1)
    Error <assertError>
      ! 1 not not equal to 1

---

    Code
      assert_that(is.atomic(mtcars))
    Error <assertError>
      ! mtcars is not an atomic vector

---

    Code
      assert_that(is.character(1:2))
    Error <assertError>
      ! 1:2 is not a character vector

---

    Code
      assert_that(is.complex(1:5))
    Error <assertError>
      ! 1:5 is not a complex vector

---

    Code
      assert_that(is.double(letters))
    Error <assertError>
      ! letters is not a numeric vector

---

    Code
      assert_that(is.integer(letters))
    Error <assertError>
      ! letters is not an integer vector

---

    Code
      assert_that(is.numeric(letters))
    Error <assertError>
      ! letters is not a numeric or integer vector

---

    Code
      assert_that(is.raw(1:10))
    Error <assertError>
      ! 1:10 is not a raw vector

---

    Code
      assert_that(is.vector(mtcars))
    Error <assertError>
      ! mtcars is not an atomic vector without attributes

---

    Code
      assert_that(is.factor(letters))
    Error <assertError>
      ! letters is not a factor

---

    Code
      assert_that(is.ordered(factor(letters)))
    Error <assertError>
      ! factor(letters) is not an ordered factor

---

    Code
      assert_that(is.array(letters))
    Error <assertError>
      ! letters is not an array

---

    Code
      assert_that(is.data.frame(1:10))
    Error <assertError>
      ! 1:10 is not a data frame

---

    Code
      assert_that(is.list("foobar"))
    Error <assertError>
      ! "foobar" is not a list

---

    Code
      assert_that(is.matrix(letters))
    Error <assertError>
      ! letters is not a matrix

---

    Code
      assert_that(is.null("not"))
    Error <assertError>
      ! "not" is not NULL

---

    Code
      assert_that(is.environment(list()))
    Error <assertError>
      ! list() is not an environment

---

    Code
      assert_that(is.function("clearly noy"))
    Error <assertError>
      ! "clearly noy" is not a function

---

    Code
      assert_that(is.primitive(assert_that))
    Error <assertError>
      ! assert_that is not a primitive function

---

    Code
      assert_that(is.call("not"))
    Error <assertError>
      ! "not" is not a quoted call

---

    Code
      assert_that(is.expression(mtcars))
    Error <assertError>
      ! mtcars is not an expression object

---

    Code
      assert_that(is.name("not really"))
    Error <assertError>
      ! "not really" is not a name

---

    Code
      assert_that(is.pairlist(1:10))
    Error <assertError>
      ! 1:10 is not a pairlist

---

    Code
      assert_that(is.recursive(1))
    Error <assertError>
      ! 1 is not a recursive object

---

    Code
      assert_that(is.symbol("almost"))
    Error <assertError>
      ! "almost" is not a name

---

    Code
      assert_that(inherits(letters, "myclass"))
    Error <assertError>
      ! letters does not inherit from class myclass

---

    Code
      assert_that(TRUE && FALSE)
    Error <assertError>
      ! FALSE is not TRUE

---

    Code
      assert_that(FALSE || FALSE)
    Error <assertError>
      ! FALSE is not TRUE or FALSE is not TRUE

---

    Code
      assert_that(any(rep(FALSE, 10)))
    Error <assertError>
      ! No elements of rep(FALSE, 10) are true

---

    Code
      assert_that(all(c(FALSE, TRUE, TRUE)))
    Error <assertError>
      ! Elements 1 of c(FALSE, TRUE, TRUE) are not true

---

    Code
      assert_that(file.exists("/file7e0fe1739e0"))
    Error <assertError>
      ! Path '/file7e0fe1739e0' does not exist

---

    Code
      assert_that(identical(1, 1L))
    Error <assertError>
      ! 1 not identical to 1L

# long call is truncated

    Code
      fail_default(call("==", strrep("-", 100), strrep("x", 100)))
    Output
      [1] "\"----------------------------------------------------------------------------------------------------\" == ... is not TRUE"

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

