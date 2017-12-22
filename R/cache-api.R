
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

#' @export

cache_list <- function(path = NULL) {
  path <- path %||% get_user_cache_dir()$root
  as_tibble(package_cache$new(path)$list())
}

#' @export

cache_find <- function(path = NULL, ...) {
  as_tibble(cache_get_file(path, target = NULL, ...))
}

#' @export

cache_get_file <- function(path = NULL, target, ...) {
  path <- path %||% get_user_cache_dir()$root
  invisible(as_tibble(package_cache$new(path)$copy_to(target, ...)))
}

#' @export

cache_delete_file <- function(path = NULL, ...) {
  path <- path %||% get_user_cache_dir()$root
  package_cache$new(path)$delete(...)
}

#' @export

cache_cleanup_all <- function(path = NULL) {
  path <- path %||% get_user_cache_dir()$root
  cache_delete_file(path)
}
