
assert_that <- function(..., env = parent.frame(), msg = NULL) {
  asserts <- eval(substitute(alist(...)))

  for (assertion in asserts) {
    res <- tryCatch({
      eval(assertion, env)
    }, assertError = function(e) {
      structure(FALSE, msg = e$message)
    })
    check_result(res)
    if (res) next

    if (is.null(msg)) {
      msg <- get_message(res, assertion, env)
      evalenv <- attr(res, "env") %||% env
    }
    throw(assert_error(
      assertion,
      res,
      msg,
      call. = sys.call(-1),
      .envir = evalenv,
    ), frame = env)
  }

  invisible(TRUE)
}

assert_error <- function(assertion, result, msg, .data = NULL, .class = NULL,
                         .envir = parent.frame(), call. = TRUE) {

  myenv <- new.env(parent = .envir)
  myenv$.arg <- if (length(assertion) >= 2) deparse(assertion[[2]])
  myenv$.arg2 <- if (length(assertion) >= 3) deparse(assertion[[3]])
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    cli::format_error(
      .envir = myenv,
      msg
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(class)) class(cnd) <- unique(c(.class, "assertError", class(cnd)))

  cnd
}
check_result <- function(x) {
  if (!is.logical(x)) {
    throw(pkg_error(
      "{.fun assert_that}: assertion must return a logical value.",
      "i" = "it returned {.type {x}} instead."
    ))
  }

  if (length(x) != 1) {
    throw(pkg_error(
      "{.fun assert_that}: assertion must return a scalar.",
      "i" = "it returned a vector of length {length(x)}."
    ))
  }

  if (any(is.na(x))) {
    throw(pkg_error(
      "{.fun assert_that}: assertion must not return {.code NA}."
    ))
  }

  TRUE
}

get_message <- function(res, call, env = parent.frame()) {
  if (has_attr(res, "msg")) {
    return(attr(res, "msg"))
  }

  f <- eval(call[[1]], env)
  if (is.call(call) && !is.primitive(f)) call <- match.call(f, call)
  fname <- deparse(call[[1]])

  fail <- base_fs[[fname]] %||% fail_default
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

has_attr <- function(x, which) {
  if (!is.null(attr(x, which, exact = TRUE))) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must have attribute {.code {which}}.",
    env = environment()
  )
}
"%has_attr%" <- has_attr

base_fs <- new.env(parent = emptyenv())

# nocov start

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

# nocov end

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

base_fs$identical <- function(call, env) {
  paste0(deparse(call$x), " not identical to ", deparse(call$y))
}
