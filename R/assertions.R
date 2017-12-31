
#' @importFrom assertthat assert_that on_failure<-
NULL

is_character <- function(x) {
  is.character(x) && ! any(is.na(x))
}

on_failure(is_character) <- function(call, env) {
  paste0(deparse(call$x), " must be a character vector without NAs")
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_string_or_null <- function(x) {
  is.null(x) || is_string(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " must be a string (length 1 character) or NULL")
}

## To be refined

is_path <- function(x) {
  is_string(x)
}

on_failure(is_path) <- function(call, env) {
  paste0(deparse(call$x), " must be a path")
}

is_path_or_null <- function(x) {
  is_string_or_null(x)
}

on_failure(is_path_or_null) <- function(call, env) {
  paste0(deparse(call$x), " must be a path or NULL")
}

## is_valid_config is in remotes.R, as the configuration parameters
## are there

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
  assert_that(is_path(x))
  file.exists(x) && ! file.info(x)$isdir
}

on_failure(is_existing_file) <- function(call, env) {
  paste0("File ", deparse(call$x), " does not exist")
}

is_platform_list <- function(x) {
  is.character(x) && length(x) > 0 && ! any(is.na(x))
}

on_failure(is_platform_list) <- function(call, env) {
  paste0(deparse(call$x), " must be a non-empty characater vector")
}

deptypes <- function() {
  c("Depends", "Suggests", "Imports", "LinkingTo", "Enhances")
}

is_dependencies <- function(x) {
  is_na_scalar(x) || isTRUE(x) || identical(x, FALSE) ||
    (is_character(x) && all(x %in% deptypes()))
}

on_failure(is_dependencies) <- function(call, env) {
  paste0(
    deparse(call$x),
    " must be TRUE, FALSE, NA or a list of dependency types"
  )
}

is_r_version_list <- function(x) {
  if (is_character(x) && length(x) > 0) {
    tryCatch({
      package_version(x)
      TRUE
    }, error = function(e) FALSE)
  } else {
    FALSE
  }
}

on_failure(is_r_version_list) <- function(call, env) {
  paste0(deparse(call$x), " must be a list or R version numbers")
}
