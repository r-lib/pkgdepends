
## nocov start

cran_metadata_cache <- NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  cran_metadata_cache <<- CRANMetadataCache$new()
}

## nocov end
