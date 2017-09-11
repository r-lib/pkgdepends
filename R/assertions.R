
#' @importFrom assertthat assert_that on_failure<-
NULL

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

all_named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}

on_failure(all_named) <- function(call, env) {
  paste0(deparse(call$x), " must be a list of named entries")
}
