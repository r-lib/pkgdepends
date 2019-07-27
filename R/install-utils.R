
#' @importFrom glue single_quote glue_collapse
collapse_quote_transformer <- function(code, envir) {
  collapse_re <- "[*]$"
  quote_re <- "^[|]"
  should_collapse <- grepl(collapse_re, code)
  should_quote <- !grepl(quote_re, code)
  code <- sub(collapse_re, "",
    sub(quote_re, "", code))
  res <- eval(parse(text = code, keep.source = FALSE), envir = envir)
  if (should_quote) {
    res <- single_quote(res)
  }
  if (should_collapse) {
    res <- glue_collapse(res, sep = ", ", last = " and ")
  }
  res
}

#' @importFrom glue glue

new_cnd_msg <- function(msg, .envir) {
  msg <- paste(msg, collapse = "")
  glue(msg, .envir = .envir, .transformer = collapse_quote_transformer)
}

new_error <- function(msg, package = NULL) {
  cnd <- err$new_error(new_cnd_msg(msg, .envir = parent.frame()))
  cnd$package <- package
  cnd
}

new_fs_error <- function(msg, package = NULL) {
  cnd <- err$new_error(new_cnd_msg(msg, .envir = parent.frame()))
  cnd$package <- package
  class(cnd) <- c("install_filesystem_error", class(cnd))
  cnd
}

new_input_error <- function(msg, package = NULL) {
  cnd <- err$new_error(new_cnd_msg(msg, .envir = parent.frame()))
  cnd$package <- package
  class(cnd) <- c("install_input_error", class(cnd))
  cnd
}

new_fs_warning <- function(msg, package = NULL) {
  cnd <- err$new_cnd(new_cnd_msg(msg, .envir = parent.frame()))
  cnd$package <- package
  class(cnd) <- c("install_filesystem_warning", "warning", class(cnd))
  cnd
}

is_loaded <- function(package) {
  package %in% loadedNamespaces()
}

create_temp_dir <- function(..., tmpdir = tempdir()) {
  f <- tempfile(tmpdir = tmpdir, ...)
  dir.create(f)
  f
}

library_cache <- function(lib) {
  lib_cache <- file.path(lib, "_cache")
  dir.create(lib_cache, recursive = TRUE, showWarnings = FALSE)
  lib_cache
}

lock_cache <- function(cache, pkg_name, lock = TRUE) {
  use_lock <- !identical(lock, FALSE)
  my_lock <- NULL
  if (use_lock) {
    lockfile <- file.path(cache, glue("{pkg_name}.lock"))
    # TODO: timeout and fail?
    my_lock <- lock(lockfile)
  }
  my_lock
}

unlock <- function(lock) {
  if (is.null(lock)) {
    return()
  }
  filelock::unlock(lock)
}


sysname <- function() {
  res <- tolower(Sys.info()[["sysname"]])
  map <- c(darwin = "mac", "sunos" = "solaris")[res]
  res[!is.na(map)] <- map
  res
}

map_lgl <- get("map_lgl", asNamespace("rlang"))

map_chr <- get("map_chr", asNamespace("rlang"))

map_int <- get("map_int", asNamespace("rlang"))

rep_list <- function(n, expr) {
  lapply(integer(n), eval.parent(substitute(function(...) expr)))
}

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}

is_count <- function(x, min = 0L)  {
  is.numeric(x) && length(x) == 1 && !is.na(x) &&
    as.integer(x) == x && x >= min
}
