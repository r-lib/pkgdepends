
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
