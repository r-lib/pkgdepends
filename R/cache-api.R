
#' Functions to query and manipulate the package cache
#'
#' `cache_summary()` returns a short summary of the state of the cache,
#' e.g. the number of files and their total size. It returns a named list.
#'
#' @param path Path of the cache. By default the cache directory is in
#'   `R-pkg`, within the user's cache directory.
#'   See [rappdirs::user_cache_dir()].
#'
#' @rdname cache_api
#' @export

cache_summary <- function(path = NULL) {
  path <- path %||% get_user_cache_dir()$root
  l <- cache_list(path)
  size <- sum(file.info(l$fullpath)$size)
  list(
    path = path,
    files = nrow(l),
    size = size
  )
}

#' `cache_list()` lists all files in the cache. It returns a `tibble`.
#'
#' @rdname cache_api
#' @export

cache_list <- function(path = NULL) {
  path <- path %||% get_user_cache_dir()$root
  as_tibble(package_cache$new(path)$list())
}

#' `cache_find()` finds all files in the cache that match the specified
#' attributes. It returns a `tibble`.
#'
#' @param ... Extra named arguments to select the package file.
#' @rdname cache_api
#' @export

cache_find <- function(path = NULL, ...) {
  as_tibble(cache_get_file(path, target = NULL, ...))
}

#' `cache_get_file()` copied a file out of the cache into the specified
#' path. If no file is found, then it returns `NULL`. Otherwise it returns
#' (invisibly) the tibble of all selected files. If multiple
#' files match the specified attributes, then the first one is copied to
#' the `target` path.
#'
#' @param target Path where the selected file is copied.
#' @rdname cache_api
#' @export

cache_get_file <- function(path = NULL, target, ...) {
  path <- path %||% get_user_cache_dir()$root
  invisible(as_tibble(package_cache$new(path)$copy_to(target, ...)))
}

#' `cache_delete_files()` deletes the selected files from the caches.
#' Without any arguments, all files are deleted.
#' @rdname cache_api
#' @export

cache_delete_files <- function(path = NULL, ...) {
  path <- path %||% get_user_cache_dir()$root
  package_cache$new(path)$delete(...)
}
