
#' @importFrom pkgbuild build
#' @importFrom utils modifyList

build_package <- function(path, build_args = list()) {
  default_args <- list(
    path = path, dest_path = NULL, binary = FALSE, vignettes = TRUE,
    manual = TRUE, args = NULL, quiet = TRUE
  )
  args <- modifyList(default_args, build_args)
  do.call(build, args)
}
