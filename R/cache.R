
detect_cache_dir <- function() {
  ## TODO
  tempfile()
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
  cdir <- get_user_metadata_cache_dir()

  tmp <- tempfile()
  mkdirp(tmp)
  l <- lock(cdir$lock, exclusive = FALSE, timeout = 10000)
  if (is.null(l)) stop("Cannot acquire lock")
  on.exit(unlock(l))
  file.copy(cdir$meta, tmp, recursive = TRUE)
  unlock(l)

  normalizePath(file.path(tmp, basename(cdir$meta)))
}

#' @importFrom rappdirs user_cache_dir

get_user_metadata_cache_dir <- function() {
  cdir <- user_cache_dir("R-pkg")
  res <- list(
    root = cdir,
    meta = file.path(cdir, "_metadata"),
    lock = file.path(cdir, "_metadata.lock")
  )
  mkdirp(res$meta)
  res
}

update_metadata_cache_dir <- function(from) {
  tryCatch(
    update_user_metadata_cache_dir(from),
    error = function(e) {
      warning("Cannot update metadata cache dir (",
              conditionMessage(e), ")")
    }
  )
}

update_user_metadata_cache_dir <- function(from) {
  cdir <- get_user_metadata_cache_dir()
  l <- lock(cdir$lock, exclusive = TRUE, timeout = 10000)
  if (is.null(l)) stop("Cannot acquire lock")
  on.exit(unlock(l))
  file.copy(from, dirname(cdir$meta), recursive = TRUE)
  unlock(l)
  NULL
}
