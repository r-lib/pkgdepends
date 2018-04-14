
## nocov start

global_metadata_cache <- NULL

#' @importFrom cli cli

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  backports::import(pkgname, "strrep")
  global_metadata_cache <<- cranlike_metadata_cache$new()
}

## nocov end
