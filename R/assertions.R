
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

is_path <- is_string

on_failure(is_path) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid path")
}

is_existing_file <- function(x) {
  file.exists(x) && ! file.info(x)$isdir
}

on_failure(is_existing_file) <- function(call, env) {
  paste0("File ", deparse(call$x), " does not exist")
}
