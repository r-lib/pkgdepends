
#' Download and build a package from GitHub
#'
#' It supports caching, both for the downloaded zip file, and the
#' built source package. The algorithm is this:
#' * Check if we have a built package in the cache. If yes, do nothing.
#' * Check if we have a downloaded zip file in the cache. If yes, build it
#'   into the cached source package.
#' * Otherwise download the zip file and build it.
#'
#' The name of the zip file is the same as the name of the source package,
#' except the extension is `.zip`.
#'
#' @param self Self.
#' @param private Private self.
#' @param resolution Dependency resolution result.
#'
#' @keywords internal

remotes__download_github <- function(self, private, resolution) {

  ref <- resolution$remote$ref
  message("Downloading ", ref)

  if (length(resolution$files) != 1) {
    stop("Invalid `files` vector, should be length one.")
  }
  files <- resolution$files[[1]]

  cache_dir <- private$get_download_cache_dir()
  target_file <- file.path(cache_dir, files$target)
  cached_zip <- sub("\\.tar\\.gz$", ".zip", target_file)
  mkdirp(dirname(target_file))
  subdir <- resolution$remote$subdir
  url <- files$source

  if (is_valid_package(target_file) || is_valid_package(target_file)) {
    had_this <- first_existing_file(target_file, target_file)
    status <- make_dl_status("Had", files, files$source, target_file,
                             bytes = file.size(had_this))
    async_constant(list(status))

  } else if (file.exists(cached_zip)) {
    build_github_package(cached_zip, target_file, subdir)
    status <- make_dl_status("Had", files, url, target_file,
                             bytes = file.size(target_file))
    async_constant(list(status))

  } else {
    download_file(url, cached_zip)$
      then(function() {
        build_github_package(cached_zip, target_file, subdir)
        list(make_dl_status("Got", files, url, target_file,
                            bytes = file.size(target_file)))
      })$
      catch(function(err) {
        error <- if (is.list(err)) err$error %||% err else err
        list(make_dl_status("Failed", files, url, target_file,
                            error = error))
      })
  }
}

build_github_package <- function(source, target, subdir) {
  mkdirp(zipdir <- tempfile())
  on.exit(unlink(zipdir, recursive = TRUE), add = TRUE)
  zipfile <- file.path(zipdir, basename(source))
  file.copy(source, zipfile)

  pkgdir <- file.path(zipdir, unzip(zipfile))[1]
  if (nzchar(subdir)) pkgdir <- file.path(pkgdir, subdir)
  pkgfile <- build_package(pkgdir)

  file.copy(pkgfile, target)
}
