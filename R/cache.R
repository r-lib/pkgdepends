
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

#' A simple package cache
#'
#' Fields:
#' * `fullpath` Full package path.
#' * `path` Package path, within the repository.
#' * `package` Package name.
#' * `url` URL it was downloaded from.
#' * `etag` ETag for the last dowload, from the given URL.
#' * `md5` MD5 of the file, to make sure if it has not changed.
#'
#' @importFrom R6 R6Class
#' @importFrom filelock lock unlock
#'
#' @keywords internal

package_cache <- R6Class(
  "package_cache",
  public = list(
    initialize = function(path = NULL) {
      assert_that(is_path(path))
      private$path <- path
      create_empty_db_file(path)
      invisible(self)
    },

    list = function() {
      l <- private$lock(exclusive = FALSE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)
      readRDS(dbfile)
    },

    find = function(path = NULL, package = NULL, url = NULL, etag = NULL,
      md5 = NULL) {
      l <- private$lock(exclusive = FALSE)
      on.exit(unlock(l), add = TRUE)
      res <- private$find_locked(path, package, url, etag, md5)
      unlock(l)
      res
    },

    add = function(file, path, package = NULL, url = NULL, etag = NULL,
      md5 = NULL) {

      assert_that(is_existing_file(file))

      l <- private$lock(exclusive = TRUE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)
      db <- readRDS(dbfile)

      idx <- find_in_data_frame(db, path = path, package = package,
                                url = url, etag = etag, md5 = md5)
      if (length(idx) != 0) stop("Package already exists in cache")

      target <- file.path(private$path, path)
      mkdirp(dirname(target))
      file.copy(file, target)
      unlink(file)
      db <- append_to_data_frame(
        db, fullpath = target, path = path, package = package, url = url,
        etag = etag, md5 = md5
      )
      saveRDS(db, dbfile)
      unlock(l)
    },

    delete = function(path = NULL, package = NULL, url = NULL,
      etag = NULL, md5 = NULL) {
      l <- private$lock(exclusive = TRUE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)

      ex <- private$find_locked(path = path)
      if (nrow(ex) == 0) stop("Package does not exist in cache")
      unlink(file.path(private$path, ex$path))
      db <- delete_from_data_frame(readRDS(dbfile), path = path)
      saveRDS(db, dbfile)
      unlock(l)
    }
  ),

  ## ----------------------------------------------------------------------

  private = list(
    path = NULL,
    lock = function(exclusive = TRUE, ...) {
      lockfile <- get_lock_file(private$path)
      filelock::lock(lockfile, exclusive = exclusive, ...)
    },
    unlock = function(l) {
      filelock::unlock(l)
    },
    find_locked = function(path = NULL, package = NULL, url = NULL,
      etag = NULL, md5 = NULL) {
      dbfile <- get_db_file(private$path)
      db <- readRDS(dbfile)

      idx <- find_in_data_frame(
        db, path = path, package = package, url = url, etag = etag,
        md5 = md5
      )
      db[idx, ]
    }
  )
)

## ------------------------------------------------------------------------
## Internal functions

get_db_file <- function(path) {
  file.path(path, ".db.rds")
}

get_lock_file <- function(path) {
  file.path(path, ".db.lock")
}

create_empty_db_file <- function(path) {
  mkdirp(path)

  dbfile <- get_db_file(path)
  lockfile <- get_lock_file(path)

  df <- make_empty_db_data_frame()

  l <- lock(lockfile)
  on.exit(unlock(l))
  saveRDS(df, file = dbfile)
}

make_empty_db_data_frame <- function() {
  data.frame(
    stringsAsFactors = FALSE,
    fullpath = character(),
    path     = character(),
    package  = character(),
    url      = character(),
    etag     = character(),
    md5      = character()
  )
}
