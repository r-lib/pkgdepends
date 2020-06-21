
assert_that <- function(..., env = parent.frame(), msg = NULL) {
  res <- see_if(..., env = env, msg = msg)
  if (res) return(TRUE)

  stop(assertError(attr(res, "msg")))
}

assertError <- function (message, call = NULL) {
  class <- c("assertError", "simpleError", "error", "condition")
  structure(list(message = message, call = call), class = class)
}

see_if <- function(..., env = parent.frame(), msg = NULL) {
  asserts <- eval(substitute(alist(...)))

  for (assertion in asserts) {
    res <- tryCatch({
      eval(assertion, env)
    }, assertError = function(e) {
      structure(FALSE, msg = e$message)
    })
    check_result(res)

    # Failed, so figure out message to produce
    if (!res) {
      if (is.null(msg))
        msg <- get_message(res, assertion, env)
      return(structure(FALSE, msg = msg))
    }
  }

  res
}

check_result <- function(x) {
  if (!is.logical(x))
    stop("assert_that: assertion must return a logical value", call. = FALSE)
  if (any(is.na(x)))
    stop("assert_that: missing values present in assertion", call. = FALSE)
  if (length(x) != 1) {
    stop("assert_that: length of assertion is not 1", call. = FALSE)
  }

  TRUE
}

get_message <- function(res, call, env = parent.frame()) {
  stopifnot(is.call(call), length(call) >= 1)

  if (has_attr(res, "msg")) {
    return(attr(res, "msg"))
  }

  f <- eval(call[[1]], env)
  if (!is.primitive(f)) call <- match.call(f, call)
  fname <- deparse(call[[1]])

  fail <- on_failure(f) %||% base_fs[[fname]] %||% fail_default
  fail(call, env)
}

# The default failure message works in the same way as stopifnot, so you can
# continue to use any function that returns a logical value: you just won't
# get a friendly error message.
# The code below says you get the first 60 characters plus a ...
fail_default <- function(call, env) {
  call_string <- deparse(call, width.cutoff = 60L)
  if (length(call_string) > 1L) {
      call_string <- paste0(call_string[1L], "...")
  }

  paste0(call_string, " is not TRUE")
}

on_failure <- function(x) attr(x, "fail")


"on_failure<-" <- function(x, value) {
  stopifnot(is.function(x), identical(names(formals(value)), c("call", "env")))
  attr(x, "fail") <- value
  x
}

path_is_not <- function(thing, var = "x") {
  function(call, env) {
    paste0("Path '", eval(call[[var]], env), "' is not ", thing)
  }
}

is.dir <- function(path) {
  assert_that(is.string(path), file.exists(path))
  file.info(path)$isdir
}
on_failure(is.dir) <- path_is_not("a directory", "path")

is.writeable <- function(path) {
  assert_that(is.string(path), file.exists(path))
  file.access(path, mode = 2)[[1]] == 0
}
on_failure(is.writeable) <- path_is_not("writeable", "path")

is.readable <- function(path) {
  assert_that(is.string(path), file.exists(path))
  file.access(path, mode = 4)[[1]] == 0
}
on_failure(is.readable) <- path_is_not("readable", "path")

has_extension <- function(path, ext) {
  tools::file_ext(path) == ext
}
on_failure(has_extension) <- function(call, env) {
  path <- eval(call$path, env)
  ext <- eval(call$ext, env)
  paste0("File '", basename(path), "' does not have extension ", ext)
}

is.scalar <- function(x) {
  length(x) == 1L
}
on_failure(is.scalar) <- function(call, env) {
  type <- eval(call$type, env)
  paste0(deparse(call$x), " is not a scalar.")
}

is.string <- function(x) is.character(x) && length(x) == 1
on_failure(is.string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (a length one character vector).")
}

is.number <- function(x) is.numeric(x) && length(x) == 1
on_failure(is.number) <- function(call, env) {
  paste0(deparse(call$x), " is not a number (a length one numeric vector).")
}

is.flag <- function(x) is.logical(x) && length(x) == 1
on_failure(is.flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (a length one logical vector).")
}

is.count <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (!is.integerish(x)) return(FALSE)

  # is.na() to handle NA_integer_
  x > 0 && !is.na(x)
}
on_failure(is.count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (a single positive integer)")
}

is.integerish <- function(x) {
  # using trunc() to deal with very large numbers (including Inf) and is.na() to deal with NaN and NA_real_
  res <- is.integer(x) || (is.numeric(x) && all(x == trunc(x)) && !is.na(x))
  res
}

# is.positive.integer
# is.negative.integer
# is.positive.double
# is.negative.double

is.named <- function(x) {
  nm <- names(x)
  !is.null(nm) && all(!is.na(nm) & nm != "")
}
on_failure(is.named) <- function(call, env) {
  paste0("Not all elements of ", deparse(call$x), " have names.")
}

has_attr <- function(x, which) !is.null(attr(x, which, exact = TRUE))
on_failure(has_attr) <- function(call, env) {
  paste0(deparse(call$x), " does not have attribute ", eval(call$which, env))
}
"%has_attr%" <- has_attr

has_name <- function(x, which){
    all(which %in% names(x))
}
on_failure(has_name) <- function(call, env) {
    out_names <- paste0("'", paste0(eval(call$which, env), collapse = "', '"), "'")
    paste0(deparse(call$x), " does not have all of these name(s): ", out_names)
}
"%has_name%" <- has_name

noNA <- function(x) {
  !(any(is.na(x)))
}
on_failure(noNA) <- function(call, env) {
  n <- sum(is.na(eval(call$x, env)))
  paste0(deparse(call$x), " contains ", n, " missing values")
}

are_equal <- function(x, y, ...) {
  isTRUE(all.equal(x, y, ...))
}
on_failure(are_equal) <- function(call, env) {
  paste0(deparse(call$x), " not equal to ", deparse(call$y))
}

is.error <- function(x) inherits(x, "try-error")
on_failure(is.error) <- function(call, env) {
  paste0(deparse(call$x), " is not a try-error")
}

is.time <- function(x) inherits(x, "POSIXt")
on_failure(is.time) <- function(call, env) {
  paste0(deparse(call$x), " is not a POSIXt date-time object")
}

is.date <- function(x) inherits(x, "Date")
on_failure(is.date) <- function(call, env) {
  paste0(deparse(call$x), " is not a Date object")
}

has_args <- function(f, args, exact = FALSE) {
  assert_that(is.function(f))

  if (exact) {
    identical(args, names(formals(f)))
  } else {
    all(args %in% names(formals(f)))
  }
}
on_failure(has_args) <- function(call, env) {
  args <- paste(eval(call$args, env), collapse = ", ")
  paste0("Function " , deparse(call$f), " does not have arguments ", args)
}

"%has_args%" <- function(f, args) has_args(f, args)

not_empty <- function(x) {
  all((dim(x) %||% length(x)) != 0)
}
on_failure(not_empty) <- function(call, env) {
  paste0(deparse(call$x), " has an empty dimension")
}

base_fs <- new.env(parent = emptyenv())

logical_is_not <- function(failed) {
  function(call, env) {
    lhs <- paste(deparse(call[[2]]), collapse = "")
    rhs <- paste(deparse(call[[3]]), collapse = "")
    paste0(lhs, " not ", failed, " ", rhs)
  }
}

base_fs$"==" <- logical_is_not("equal to")
base_fs$"<" <-  logical_is_not("less than")
base_fs$">" <-  logical_is_not("greater than")
base_fs$">=" <- logical_is_not("greater than or equal to")
base_fs$"<=" <- logical_is_not("less than or equal to")
base_fs$"!=" <- logical_is_not("not equal to")

is_not <- function(thing) {
  function(call, env) {
    paste0(deparse(call[[2]]), " is not ", thing)
  }
}

# Vectors
base_fs$is.atomic <- is_not("an atomic vector")
base_fs$is.character <- is_not("a character vector")
base_fs$is.complex <- is_not("a complex vector")
base_fs$is.double <- is_not("a numeric vector")
base_fs$is.integer <- is_not("an integer vector")
base_fs$is.numeric <- is_not("a numeric or integer vector")
base_fs$is.raw <- is_not("a raw vector")
base_fs$is.vector <- is_not("an atomic vector without attributes")

# Factors
base_fs$is.factor <- is_not("a factor")
base_fs$is.ordered <- is_not("an ordered factor")

# More complicated data structures
base_fs$is.array <- is_not("an array")
base_fs$is.data.frame <- is_not("a data frame")
base_fs$is.list <- is_not("a list")
base_fs$is.matrix <- is_not("a matrix")
base_fs$is.null <- is_not("NULL")

# Functions and environments
base_fs$is.environment <- is_not("an environment")
base_fs$is.function <- is_not("a function")
base_fs$is.primitive <- is_not("a primitive function")

# Computing on the language
base_fs$is.call <- is_not("a quoted call")
base_fs$is.expression <- is_not("an expression object")
base_fs$is.name <- is_not("a name")
base_fs$is.pairlist <- is_not("a pairlist")
base_fs$is.recursive <- is_not("a recursive object")
base_fs$is.symbol <- is_not("a name")

# Catch all
base_fs$inherits <- function(call, env) {
  class <- eval(call$what, env)
  paste0(deparse(call$x), " does not inherit from class ", class)
}

base_fs$"&&" <- function(call, env) {
  lhs <- eval(call[[2]], env)
  if (!lhs) {
    get_message(lhs, call[[2]], env)
  } else {
    rhs <- eval(call[[3]], env)
    get_message(rhs, call[[3]], env)
  }
}

base_fs$"||" <- function(call, env) {
  lhs <- eval(call[[2]], env)
  l_msg <- get_message(lhs, call[[2]], env)

  rhs <- eval(call[[3]], env)
  r_msg <- get_message(rhs, call[[3]], env)

  paste0(l_msg, " or ", r_msg)
}

base_fs$any <- function(call, env) {
  paste0("No elements of ", deparse(call[[2]]), " are true")
}

base_fs$all <- function(call, env) {
  res <- eval(call[[2]], env)
  i <- which(!res)
  if (length(i) > 10) i <- c(i[1:5], "...")

  paste0("Elements ", paste(i, collapse = ", "), " of ",
    deparse(call[[2]]), " are not true")
}

base_fs$file.exists <- function(call, env) {
  path <- eval(call[[2]], env)
  paste0("Path '", path, "' does not exist")
}

base_fs$anyDuplicated <- function(call, env) {
  paste0(call$x, " is not unique")
}

base_fs$identical <- function(call, env) {
  paste0(deparse(call$x), " not identical to ", deparse(call$y))
}

"%||%" <- function(a, b) if (is.null(a)) b else a

validate_that <- function(..., env = parent.frame(), msg = NULL) {
  res <- see_if(..., env = env, msg = msg)
  if (res) return(TRUE)
  return(attr(res, "msg"))
}
