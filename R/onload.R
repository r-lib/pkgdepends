
## nocov start

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  backports::import(pkgname, "strrep")
  err$onload_hook()
  lazyrmd$onload_hook(
    local = "if-newer",
    ci = function() has_asciicast_support() && getRversion() >= "3.3",
    cran = FALSE
  )
}

## nocov end
