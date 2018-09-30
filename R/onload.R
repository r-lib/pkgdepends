
## nocov start

#' @importFrom cliapp cliapp

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
  backports::import(pkgname, "strrep")
}

## nocov end
