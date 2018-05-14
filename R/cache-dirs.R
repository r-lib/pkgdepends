
detect_download_cache_dir <- function() {
  tempfile()
}

detect_package_cache_dir <- function() {
  tryCatch(
    get_user_cache_dir()$root,
    error = function(e) {
      warning(
        "Cannot set package cache directory, using temporary directory. (",
        conditionMessage(e), ")"
      )
      tmp <- tempfile()
      mkdirp(tmp)
      normalizePath(tmp)
    }
  )
}

#' @importFrom rappdirs user_cache_dir

get_user_cache_dir <- function() {
  cdir <- user_cache_dir("R-pkg")
  res <- list(
    root = cdir,
    meta = file.path(cdir, "_metadata"),
    lock = file.path(cdir, "_metadata.lock")
  )
  mkdirp(res$meta)
  res
}
