
## nocov start

cran_metadata_cache <- NULL
cli_theme_id <- NULL

#' @importFrom cli cli

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  backports::import(pkgname, "strrep")
  cran_metadata_cache <<- CRANMetadataCache$new()
  cli_theme_id <<- cli$add_theme(pkg_theme())
}

.onUnload <- function(libpath) {
  if (!is.null(cli_theme_id)) cli$remove_theme(cli_theme_id)
}

## nocov end
