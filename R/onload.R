
## nocov start

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  backports::import(pkgname, "strrep")
  err$onload_hook()
  lazyrmd$onload_hook(
    local = "if-newer",
    ci = TRUE,
    cran = FALSE
  )
}

## nocov end
