
default_remote_types <- function() {
  default <- list(
    cran = list(
      parse = parse_remote_cran,
      resolve = resolve_remote_cran,
      download = resolve_remote_cran,
      satisfy = satisfy_remote_cran),
    bioc = list(
      parse = parse_remote_bioc,
      resolve = resolve_remote_bioc,
      download = resolve_remote_bioc,
      satisfy = satisfy_remote_bioc),
    standard = list(
      parse = parse_remote_standard,
      resolve = resolve_remote_standard,
      download = resolve_remote_standard,
      satisfy = satisfy_remote_standard),
    github = list(
      parse = parse_remote_github,
      resolve = resolve_remote_github,
      download = download_remote_github),
    local = list(
      parse = parse_remote_local,
      resolve = resolve_remote_local,
      download = resolve_remote_local,
      satisfy = satisfy_remote_local),
    installed = list(
      parse = parse_remote_installed,
      resolve = resolve_remote_installed,
      download = resolve_remote_installed,
      satisfy = satisfy_remote_installed)
  )

  modifyList(default, as.list(getOption("pkg.remote_types")))
}

`$.remote_resolution` <- function(x, name) {
  stop("Internal error, no direct access, please use a method")
}

get_files <- function(x)
  UseMethod("get_files")

#' @export

get_files.remote_resolution <- function(x) {
  x[["files"]]
}

set_files <- function(x, f)
  UseMethod("set_files")

#' @export

set_files.remote_resolution <- function(x, f) {
  x[["files"]] <- f
  st <- vcapply(get_files(x), "[[", "status")
  x[["status"]] <- if (all(st == "OK")) "OK" else "FAILED"
  x
}

add_files <- function(x, f)
  UseMethod("add_files")

#' @export

add_files.remote_resolution <- function(x, f) {
  x[["files"]] <- c(x[["files"]], list(f))
  st <- vcapply(get_files(x), "[[", "status")
  x[["status"]] <- if (all(st == "OK")) "OK" else "FAILED"
  x
}

get_status <- function(x)
  UseMethod("get_status")

#' @export

get_status.remote_resolution <- function(x) {
  if (!is.null(st <- x[["status"]])) return(st)
  if (all(vcapply(get_files(x), "[[", "status") == "OK")) "OK" else "FAILED"
}

num_files <- function(x)
  UseMethod("num_files")

#' @export

num_files.remote_resolution <- function(x) {
  length(get_files(x))
}

get_remote <- function(x)
  UseMethod("get_remote")

#' @export

get_remote.remote_resolution <- function(x) {
  x[["remote"]]
}

get_ref <- function(x)
  UseMethod("get_ref")

#' @export

get_ref.remote_resolution <- function(x) {
  get_remote(x)$ref
}

get_error_message <- function(x)
  UseMethod("get_error_message")

as_error_msg <- function(x) {
  if (is_string(x)) {
    x
  } else if (inherits(x, "condition")) {
    x$message
  } else if (is.list(x)) {
    unique(vcapply(x, as_error_msg))
  } else {
    "Unknown error"
  }
}

#' @export

get_error_message.remote_resolution <- function(x) {
  x[["error"]]$message %||%
    unlist(lapply(lapply(get_files(x), "[[", "error"), as_error_msg)) %||% "???"
}

get_direct <- function(x)
  UseMethod("get_direct")

#' @export

get_direct.remote_resolution <- function(x) {
  x[["direct"]]
}
