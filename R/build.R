
#' Build binary packages and place them in the repository
#'
#' @param packages Names of the packages.
#' @param repo Path to the repository. Defaults to [default_repo()].
#'
#' @export

build_binary <- function(packages, repo = default_repo()) {
  ## TODO
}

#' @importFrom withr with_dir
#' @importFrom callr rcmd_safe

build_package <- function(path) {

  path <- normalizePath(path)
  pkgdir <- dirname(path)

  ## If not a tar.gz, build it. Otherwise just leave it as it is.
  build_status <- with_dir(
    pkgdir,
    rcmd_safe("build", basename(path))
  )
  if (build_status$status != 0) {
    stop("Build failed for ", sQuote(basename(path)))
  }

  file.path(
    pkgdir,
    list.files(pkgdir, pattern = "\\.tar\\.gz$")
  )
}
