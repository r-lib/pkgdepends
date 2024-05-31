# is_character errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_character(x))
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a character vector without `NA`, but it is an integer vector
      [90mType .Last.error to see the more details.[39m
      > fn(c("", NA_character_))
      [1m[33mError[39m[22m in `fn(c("", NA_character_))`:
      [33m![39m [1m[22m`x` must be a character vector without `NA`, but it has 1 `NA` value.
      [90mType .Last.error to see the more details.[39m
      > fn(rep(NA_character_, 5))
      [1m[33mError[39m[22m in `fn(rep(NA_character_, 5))`:
      [33m![39m [1m[22m`x` must be a character vector without `NA`, but it has 5 `NA` values.
      [90mType .Last.error to see the more details.[39m

# is_character errors, noninteractive

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_character(x))
      > options(cli.unicode = FALSE)
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a character vector without `NA`, but it is an integer vector
      ---
      Backtrace:
      [90m1. [39mglobal [1mfn[22m[38;5;178m([38;5;169m1[38;5;178m:[38;5;169m2[38;5;178m)[39m
      [90m2. | pkgdepends:::assert_that(is_character(x))[39m
      [90m3. | pkgdepends:::throw(assert_error(assertion, res, msg, call. = sys.call(-1), ...[39m
      Execution halted

---

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_character(x))
      > options(cli.unicode = FALSE)
      > fn(c("", NA_character_))
      [1m[33mError[39m[22m in `fn(c("", NA_character_))`:
      [33m![39m [1m[22m`x` must be a character vector without `NA`, but it has 1 `NA` value.
      ---
      Backtrace:
      [90m1. [39mglobal [1mfn[22m[38;5;178m([39m[1mc[22m[33m([39m[38;5;37m""[39m, [38;5;169mNA_character_[39m[33m)[39m[38;5;178m)[39m
      [90m2. | pkgdepends:::assert_that(is_character(x))[39m
      [90m3. | pkgdepends:::throw(assert_error(assertion, res, msg, call. = sys.call(-1), ...[39m
      Execution halted

---

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_character(x))
      > options(cli.unicode = FALSE)
      > fn(rep(NA_character_, 5))
      [1m[33mError[39m[22m in `fn(rep(NA_character_, 5))`:
      [33m![39m [1m[22m`x` must be a character vector without `NA`, but it has 5 `NA` values.
      ---
      Backtrace:
      [90m1. [39mglobal [1mfn[22m[38;5;178m([39m[1mrep[22m[33m([39m[38;5;169mNA_character_[39m, [38;5;169m5[39m[33m)[39m[38;5;178m)[39m
      [90m2. | pkgdepends:::assert_that(is_character(x))[39m
      [90m3. | pkgdepends:::throw(assert_error(assertion, res, msg, call. = sys.call(-1), ...[39m
      Execution halted

# is_string errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_string(x))
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a string (character scalar), but it is an integer vector.
      [90mType .Last.error to see the more details.[39m
      > fn("foo")
      > fn(NA_character_)
      [1m[33mError[39m[22m in `fn(NA_character_)`:
      [33m![39m [1m[22m`x` must not be `NA`.
      [90mType .Last.error to see the more details.[39m
      > fn(NULL)
      [1m[33mError[39m[22m in `fn(NULL)`:
      [33m![39m [1m[22m`x` must be a string (character scalar), but it is NULL.
      [90mType .Last.error to see the more details.[39m

# is_optional_string errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_optional_string(x))
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a path (character scalar), but it is an integer vector.
      [90mType .Last.error to see the more details.[39m
      > fn(c("foo", "bar"))
      [1m[33mError[39m[22m in `fn(c("foo", "bar"))`:
      [33m![39m [1m[22m`x` must be a path (character scalar), but it is a character vector.
      [90mType .Last.error to see the more details.[39m
      > fn(NA_character_)
      [1m[33mError[39m[22m in `fn(NA_character_)`:
      [33m![39m [1m[22m`x` must be a path (character scalar), but it is a character `NA`.
      [90mType .Last.error to see the more details.[39m

# is_flag errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_flag(x))
      > fn(NA)
      [1m[33mError[39m[22m in `fn(NA)`:
      [33m![39m [1m[22m`x` must not be `NA`.
      [90mType .Last.error to see the more details.[39m
      > fn(1:10)
      [1m[33mError[39m[22m in `fn(1:10)`:
      [33m![39m [1m[22m`x` must be a flag (logical scalar), but it is an integer vector.
      [90mType .Last.error to see the more details.[39m

# is_path errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_path(x))
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a path (character scalar), but it is an integer vector.
      [90mType .Last.error to see the more details.[39m
      > fn(c("foo", "bar"))
      [1m[33mError[39m[22m in `fn(c("foo", "bar"))`:
      [33m![39m [1m[22m`x` must be a path (character scalar), but it is a character vector.
      [90mType .Last.error to see the more details.[39m
      > fn(NA_character_)
      [1m[33mError[39m[22m in `fn(NA_character_)`:
      [33m![39m [1m[22m`x` must be a path (character scalar), but it is a character `NA`.
      [90mType .Last.error to see the more details.[39m

# is_optional path errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_optional_path(x))
      > fn(1:2)
      [1m[33mError[39m[22m in `fn(1:2)`:
      [33m![39m [1m[22m`x` must be a path (character scalar) or `NULL`, but it is an integer
      vector.
      [90mType .Last.error to see the more details.[39m
      > fn(c("foo", "bar"))
      [1m[33mError[39m[22m in `fn(c("foo", "bar"))`:
      [33m![39m [1m[22m`x` must be a path (character scalar) or `NULL`, but it is a character
      vector.
      [90mType .Last.error to see the more details.[39m
      > fn(NA_character_)
      [1m[33mError[39m[22m in `fn(NA_character_)`:
      [33m![39m [1m[22m`x` must be a path (character scalar) or `NULL`, but it is a character
      `NA`.
      [90mType .Last.error to see the more details.[39m

# all_named errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(all_named(x))
      > fn(c(a = 1, 2))
      [1m[33mError[39m[22m in `fn(c(a = 1, 2))`:
      [33m![39m [1m[22mAll elements in `x` must be named.
      [90mType .Last.error to see the more details.[39m

# is_existing_file errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_existing_file(x))
      > fn("file1500454880b58")
      [1m[33mError[39m[22m in `fn("file1500454880b58")`:
      [33m![39m [1m[22mPath [34mfile1500454880b58[39m (from `x`) does not exist
      [90mType .Last.error to see the more details.[39m
      > fn(".")
      [1m[33mError[39m[22m in `fn(".")`:
      [33m![39m [1m[22mFile [34m.[39m (from `x`) must be a regular file, not a directory
      [90mType .Last.error to see the more details.[39m

# is_platform_list errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_platform_list(x))
      > fn(1:10)
      [1m[33mError[39m[22m in `fn(1:10)`:
      [33m![39m [1m[22m`x` must be a list of platforms, a non-empty character vector, but it is
      an integer vector
      [90mType .Last.error to see the more details.[39m
      > fn(character())
      [1m[33mError[39m[22m in `fn(character())`:
      [33m![39m [1m[22m`x` must be a list of platforms, a non-empry character vector
      [90mType .Last.error to see the more details.[39m
      > fn(NA_character_)
      [1m[33mError[39m[22m in `fn(NA_character_)`:
      [33m![39m [1m[22m`((.arg)`} must be a list of platforms, a character vector without
      missing values, but it has 1 missing value.
      [90mType .Last.error to see the more details.[39m

# is_dependencies errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_dependencies(x))
      > options(cli.unicode = FALSE)
      > fn(c("Depends", NA))
      [1m[33mError[39m[22m in `fn(c("Depends", NA))`:
      [33m![39m [1m[22m`x` must be one of the following: `NA`, `TRUE`, `FALSE`, a character
      vector of dependency types, a named list with entries `direct` and `indirect`,
      both character vectors of dependency types.
      [36mi[39m valid dependency types are: [34m"Depends"[39m, [34m"Imports"[39m, [34m"LinkingTo"[39m, [34m"Suggests"[39m,
        [34m"Enhances"[39m, [34m"soft"[39m, [34m"hard"[39m, and [34m"all"[39m, and `config/needs/*` types
      [90mType .Last.error to see the more details.[39m

# is_r_version_list errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_r_version_list(x))
      > fn(1:10)
      [1m[33mError[39m[22m in `fn(1:10)`:
      [33m![39m [1m[22m`x` must be a list of R versions, a character vector, but it is an
      integer vector.
      [90mType .Last.error to see the more details.[39m
      > fn(character())
      [1m[33mError[39m[22m in `fn(character())`:
      [33m![39m [1m[22m`x` must be a non-empty list of R versions, a character vector, but it
      is empty.
      [90mType .Last.error to see the more details.[39m
      > fn(c("1.2", "foo", "bar", "1.2.0"))
      [1m[33mError[39m[22m in `lapply(text, glue_cmd, .envir = .envir)`:
      [33m![39m [1m[22mCould not evaluate cli `{}` expression: `sum(is.na(cv))`.
      [1mCaused by error[22m in `eval(expr, envir = envir)`:
      [33m![39m object 'cv' not found
      [90mType .Last.error to see the more details.[39m

# is_difftime errors

    Code
      r_process()
    Output
      > fn <- function(x) assert_that(is_difftime(x))
      > fn(1)
      [1m[33mError[39m[22m in `fn(1)`:
      [33m![39m [1m[22m`x` must be a [34m<difftime>[39m object, but it is a number.
      [90mType .Last.error to see the more details.[39m

# is_count errors

    Code
      fn(letters)
    Condition
      Error:
      ! `x` must be a count, a non-negative integer scalar.
      i It is a character vector.

---

    Code
      fn(1:10)
    Condition
      Error:
      ! `x` must be a count, a non-negative integer scalar.
      i It is an integer vector.

---

    Code
      fn(-1)
    Condition
      Error:
      ! `x` must be at least 0.

---

    Code
      fn(0, min = 1)
    Condition
      Error:
      ! `x` must be at least 1.

---

    Code
      fn(NA_integer_)
    Condition
      Error:
      ! `x` must not be a missing value (`NA`).

