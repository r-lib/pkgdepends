
## nocov start

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  err$onload_hook()
}

## nocov end
