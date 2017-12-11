
parse_remote <- function(specs, config, ...)
  UseMethod("parse_remote")

parse_remote.default <- function(specs, config, ...)
  stop("Unknown or incomplete remote specs type, no `parse_remote` method")

resolve_remote <- function(remote, config, ...)
  UseMethod("resolve_remote")

resolve_remote.default <- function(remote, config, ...)
  stop("Unknown or incomplete remote type, no `resolve_remote` method")

download_remote <- function(resolution, config, ...)
  UseMethod("download_remote")

download_remote.default <- function(resolution, config, ...)
  stop("Unknown or incomplete remote type, no `download_remote` method")

satisfies_remote <- function(resolution, installed_description, config, ...)
  UseMethod("satisfies_remote")

satisfies_remote.default <- function(resolution, candidate, config, ...) {
  FALSE
}

`$.remote_resolution` <- function(x, name) {
  stop("Internal error, no direct access, please use a method")
}

get_files <- function(x)
  UseMethod("get_files")

get_files.remote_resolution <- function(x) {
  x[["files"]]
}

set_files <- function(x, f)
  UseMethod("set_files")

set_files.remote_resolution <- function(x, f) {
  x[["files"]] <- f
  x
}

get_status <- function(x)
  UseMethod("get_status")

get_status.remote_resolution <- function(x) {
  if (!is.null(st <- x[["status"]])) return(st)
  if (all(vcapply(get_files(x), "[[", "status") == "OK")) "OK" else "FAILED"
}

num_files <- function(x)
  UseMethod("num_files")

num_files.remote_resolution <- function(x) {
  length(get_files(x))
}

get_remote <- function(x)
  UseMethod("get_remote")

get_remote.remote_resolution <- function(x) {
  x[["remote"]]
}

get_ref <- function(x)
  UseMethod("get_ref")

get_ref.remote_resolution <- function(x) {
  get_remote(x)$ref
}

get_error_message <- function(x)
  UseMethod("get_error_message")

get_error_message.remote_resolution <- function(x) {
  x[["error"]]$message
}
