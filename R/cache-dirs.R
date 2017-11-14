
detect_cache_dir <- function() {
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

detect_metadata_cache_dir <- function() {
  tryCatch(
    user_metadata_cache_dir(),
    error = function(e) {
      warning(
        "Cannot set metadata cache directory, using temporary directory. (",
        conditionMessage(e), ")"
      )
      tmp <- tempfile()
      mkdirp(tmp)
      normalizePath(tmp)
    }
  )
}

#' Cache directory for the CRAN (or other) metadata
#'
#' We want `pkgdepends` to work with concurrent R sessions, so we need
#' to keep the metadata consistent. So we use a lock, and make a copy
#' of the directory, to avoid holding the lock for a long time.
#' This way we don't hold up other processes. Then we update the cache
#' after acquiring a write lock.
#'
#' For `repoman` we do not use these caches, but instead cache everythin
#' within the repository.
#'
#' @return path
#'
#' @importFrom filelock lock unlock
#'
#' @keywords internal

user_metadata_cache_dir <- function() {
  cdir <- get_user_cache_dir()

  tmp <- file.path(tempfile(), basename(cdir$meta))
  mkdirp(tmp)
  l <- lock(cdir$lock, exclusive = FALSE, timeout = 10000)
  if (is.null(l)) stop("Cannot acquire lock")
  on.exit(unlock(l))
  file.copy(cdir$meta, dirname(tmp), recursive = TRUE)
  unlock(l)
  normalizePath(tmp)
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

update_metadata_cache <- function(root, files) {
  tryCatch(
    update_user_metadata_cache(root, files),
    error = function(e) {
      warning("Cannot update metadata cache dir (",
              conditionMessage(e), ")")
    }
  )
}

update_user_metadata_cache <- function(root, files) {
  cdir <- get_user_cache_dir()
  l <- lock(cdir$lock, exclusive = TRUE, timeout = 10000)
  if (is.null(l)) stop("Cannot acquire lock")
  on.exit(unlock(l))
  for (f in files) {
    target_dir <- file.path(cdir$meta, dirname(f))
    mkdirp(target_dir)
    file.copy(file.path(root, f), target_dir, recursive = TRUE)
  }
  unlock(l)
  NULL
}
