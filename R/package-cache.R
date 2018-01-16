
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

    find = function(..., .list = NULL) {
      self$copy_to(NULL, ..., .list = .list)
    },

    copy_to = function(target, ..., .list = NULL) {
      l <- private$lock(exclusive = FALSE)
      on.exit(unlock(l), add = TRUE)
      res <- private$find_locked(..., .list = .list)
      if (!is.null(target) && nrow(res) >= 1) {
        file.copy(res$fullpath[1], target)
      }
      res
    },

    add = function(file, path, ..., .list = NULL) {

      assert_that(is_existing_file(file))

      l <- private$lock(exclusive = TRUE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)
      db <- readRDS(dbfile)

      idx <- find_in_data_frame(db, path = path, ..., .list = .list)
      if (length(idx) != 0) stop("Package already exists in cache")

      target <- file.path(private$path, path)
      mkdirp(dirname(target))
      file.copy(file, target)
      db <- append_to_data_frame(db, fullpath = target, path = path, ...,
                                 .list = .list)
      saveRDS(db, dbfile)
    },

    delete = function(..., .list = NULL) {
      l <- private$lock(exclusive = TRUE)
      on.exit(unlock(l), add = TRUE)
      dbfile <- get_db_file(private$path)

      ex <- private$find_locked(..., .list = .list)
      if (nrow(ex) == 0) stop("Package does not exist in cache")
      unlink(file.path(private$path, ex$path))
      db <- delete_from_data_frame(readRDS(dbfile), ..., .list = .list)
      saveRDS(db, dbfile)
    }
  ),

  ## ----------------------------------------------------------------------

  private = list(
    path = NULL,
    lock = function(exclusive = TRUE, ...) {
      lockfile <- get_lock_file(private$path)
      filelock::lock(lockfile, exclusive = exclusive, ...)
    },
    find_locked = function(..., .list = NULL) {
      dbfile <- get_db_file(private$path)
      db <- readRDS(dbfile)

      idx <- find_in_data_frame(db, ..., .list = NULL)
      db[idx, ]
    }
  )
)

#' Get package from cache, or download it asynchronously
#'
#' Currently, even if we take it from the cache, we check the etag
#' of the url. TODO: This should change in the future, and we should trust
#' at least CRAN source packages.
#'
#' @param cache `package_cache` instance.
#' @param urls Character vector, list of candidate urls.
#' @param target_dir Directory to place the file in.
#' @param target Path to target file, within the target directory.
#' @param progress_bar The progress bar object to update.
#' @param direct Whether the package/ref was directly specified
#'   (or a dependency).
#' @param metadata Extra data to add to the installed package.
#' @return Download status.
#'
#' @keywords internal

get_package_from <- function(cache, urls, target_dir, target,
                             progress_bar = NULL, direct = NULL,
                             metadata = list()) {
  cache ; urls ; metadata
  target_file <- file.path(target_dir, target)
  mkdirp(target_dir <- dirname(target_file))

  etag_file <- tempfile()
  for (url in urls) {
    hit <- cache$copy_to(target_file, url = url, .list = metadata)
    if (nrow(hit) >= 1) {
      writeLines(hit$etag, etag_file)
      break
    }
  }

  headers <- if (!is.null(direct)) {
    c("User-Agent" = paste0(
        getOption("HTTPUserAgent"), "; ",
        "pkgdepends ", getNamespaceVersion("pkgdepends"), "; ",
        "curl ", getNamespaceVersion("curl"), "; ",
        if (isTRUE(direct)) "direct" else "indirect"))
  }

  download_try_list(urls, target_file, etag_file,
                    headers = as.character(headers),
                    progress_bar = progress_bar)$
    then(function(status) {
      if (status == 304) {
        make_dl_status("Had", urls, target_file,
                       bytes = file.size(target_file))
      } else {
        etag <- read_etag(etag_file)
        metadata <- metadata %||% list()
        metadata$package <- metadata$package %||% NA_character_
        metadata$md5 <- metadata$md5 %||% NA_character_
        cache$add(target_file, path = target, url = urls[1], # TODO: url!
                  etag = etag, .list = metadata)
        make_dl_status("Got", urls, target_file,
                       bytes = file.size(target_file))
      }
    })$
    catch(function(err) {
      make_dl_status("Failed", urls, target_file,  error = err)
    })
}

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
    path     = character()
  )
}
