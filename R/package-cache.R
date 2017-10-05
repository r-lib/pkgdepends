
#' Get package from cache, or download it asynchronously
#'
#' Currently, even if we take it from the cache, we check the etag
#' of the url. TODO: This should change in the future, and we should trust
#' at least CRAN source packages.
#'
#' @param cache `package_cache` instance.
#' @param urls Character vector, list of candidate urls.
#' @param target_file Where to save the file.
#' @return Download status.
#'
#' @keywords internal
#' @importFrom async async_detect

get_package_from <- function(cache, urls, cache_dir, target) {
  cache ; urls ; cache_dir; target
  target_file <- file.path(cache_dir, target)
  mkdirp(target_dir <- dirname(target_file))

  etag_file <- tempfile()
  for (url in urls) {
    hit <- cache$copy_to(target_file, url = url)
    if (nrow(hit) >= 1) {
      writeLines(hit$etag, etag_file)
      break
    }
  }

  download_try_list(urls, target_file, etag_file)$
    then(function(status) {
      if (status == 304) {
        make_dl_status("Had", urls, target_file,
                       bytes = file.size(target_file))
      } else {
        etag <- read_etag(etag_file)
        cache$add(target_file, path = target, package = NA_character_,
                  url = urls[1], etag = etag, md5 = NA_character_)
        make_dl_status("Got", urls, target_file,
                       bytes = file.size(target_file))
      }
    })$
    catch(function(err) {
      make_dl_status("Failed", urls, target_file,  error = err)
    })
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
      create_empty_db_file_if_needed(path)
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
      self$copy_to(NULL, path, package, url, etag, md5)
    },

    copy_to = function(target, path = NULL, package = NULL, url = NULL,
      etag = NULL, md5 = NULL) {
      l <- private$lock(exclusive = FALSE)
      on.exit(unlock(l), add = TRUE)
      res <- private$find_locked(path, package, url, etag, md5)
      if (!is.null(target) && nrow(res) >= 1) {
        file.copy(res$fullpath[1], target)
      }
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

create_empty_db_file_if_needed <- function(path) {
  mkdirp(path)

  dbfile <- get_db_file(path)
  if (file.exists(dbfile)) return()

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
