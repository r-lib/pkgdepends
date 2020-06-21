
get_platform <- function() {
  .Platform
}

#' @export
#' @rdname default_platforms

current_r_platform <- function() {
  type <- get_platform()$pkgType
  if (!is_string(type))
    "source"
  else if (grepl("^mac", type)) {
    "macos"
  } else if (grepl("^win", type)) {
    "windows"
  } else {
    "source"
  }
}

#' The current R platform for packages
#'
#' `current_r_platform()` detects the current platform.
#' `default_platforms()` prints the default package types that are used
#' on the current platform. See also ['Configuration'][pkg_config].
#'
#' @return `current_r_platform()` returns a string:
#'   `source`, `macos` or `windows`.
#'
#'   `default_platforms()` return a character vector of package types that
#'   work on the current system. It is a subset of the possible
#'   `current_r_platform()` return values.
#'
#' @family platform functions
#' @export
#' @examples
#' current_r_platform()
#' default_platforms()

default_platforms <- function() unique(c(current_r_platform(), "source"))
