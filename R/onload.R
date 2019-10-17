
## nocov start

deferred <- NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  backports::import(pkgname, "strrep")
  deferred <<- asNamespace("pkgcache")$deferred
}

## nocov end
