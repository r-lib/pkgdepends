
parse_remote <- function(specs)
  UseMethod("parse_remote")

parse_remote.default <- function(specs)
  stop("Unknown or incomplete remote type, no `parse_remote` method")

resolve_remote <- function(remote)
  UseMethod("resolve_remote")

resolve_remote.default <- function(remote)
  stop("Unknown or incomplete remote type, no `resolve_remote` method")

download_remote <- function(resolution)
  UseMethod("download_remote")

download_remote.default <- function(resolution)
  stop("Unknown or incomplete remote type, no `download_remote` method")
