
cran_metadata_cache <- NULL

.onLoad <- function(libname, pkgname) {
  cran_metadata_cache <<- CRANMetadataCache$new()
}
