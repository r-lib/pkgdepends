#' Scan R code for dependent packages
#'
#' @keywords internal

scan_deps <- function(path = ".") {
  paths <- dir(path, pattern = "[.](R|r|Rmd|rmd)$", recursive = TRUE)
  full_paths <- normalizePath(file.path(path, paths))
  deps_list <- lapply(full_paths, scan_path_deps)
  deps <- do.call("rbind", c(list(scan_path_deps_empty()), deps_list))
  # write back the relative paths
  deps$path <- paths[match(deps$path, full_paths)]
  deps
}

# -------------------------------------------------------------------------

# needs to increase as the deps discovry code changes, otherwise we don't
# apply the new discovery code
deps_cache_version <- 1L

get_deps_cache_path <- function(hash = NULL) {
  root <- file.path(get_user_cache_dir()$root, "deps", deps_cache_version)
  if (is.null(hash)) {
    root
  } else {
    file.path(root, substr(hash, 1, 2), hash)
  }
}

clear_deps_cache <- function() {
  unlink(dirname(get_deps_cache_path()), recursive = TRUE)
}

scan_path_deps <- function(path) {
  code <- readBin(path, "raw", file.size(path))

  # check if already known, set path
  hash <- cli::hash_raw_xxhash(code)
  cache <- get_deps_cache_path(hash)
  if (file.exists(cache)) {
    deps <- readRDS(cache)
    if (!is.null(deps) && nrow(deps) > 0) {
      deps$path <- path
    }
    return(deps)
  }

  # scan it if it is worth it, based on a quick check
  has_deps <- length(grepRaw("library|require|loadNamespace|::", code)) > 0
  deps <- if (has_deps) scan_path_deps_do(code, path)

  # save it to the cache, but anonimize it first. If no deps, save NULL
  deps_no_path <- deps
  if (!is.null(deps_no_path) && nrow(deps_no_path) > 0) {
    deps_no_path$path <- ""
  }
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(deps_no_path, cache)

  deps
}

scan_path_deps_empty <- function() {
  data_frame(
    path = character(),
    package = character(),
    code = character(),
    start_row = integer(),
    start_column = integer(),
    start_byte = integer()
  )
}

scan_path_deps_do <- function(code, path) {
  ext <- tolower(file_ext(path))
  switch(
    ext,
    ".r" = scan_path_deps_do_r(code, path),
    ".qmd" = ,
    ".rmd" = scan_path_deps_do_rmd(code, path),
    stop("Cannot parse ", ext, " file for dependencies, internal error")
  )
}

# -------------------------------------------------------------------------

scan_path_deps_do_r <- function(code, path, ranges = NULL) {
  hits <- code_query(code, q_deps(), ranges = ranges)
  # q_library_0 hits are generic ones, only use them if they are not hit
  gen_pat <- hits$patterns$id[hits$patterns$name == "q_library_0"]
  gen_hits <- hits$matched_captures[hits$matched_captures$pattern %in% gen_pat, ]
  ok_hits <- hits$matched_captures[! hits$matched_captures$pattern %in% gen_pat, ]
  rbind(
    if (nrow(ok_hits) > 0) scan_path_deps_do_ok_hits(ok_hits, path),
    if (nrow(gen_hits) > 0) scan_path_deps_do_gen_hits(gen_hits, path)
  )
}

scan_path_deps_do_ok_hits <- function(hits, path) {
  data_frame(
    path = path,
    package = hits$code[hits$name == "pkg-name"],
    code = hits$code[hits$name == "dep-code"],
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_path_deps_do_gen_hits <- function(hits, path) {
  code <- hits$code[hits$name == "dep-code"]
  fn <- hits$code[hits$name == "fn-name"]
  pkgs <- vcapply(seq_along(code), function(i) {
    parse_pkg_from_library_call(fn[i], code[i])
  })
  ok <- !is.na(pkgs)
  data_frame(
    path = path,
    package = pkgs[ok],
    code = code[ok],
    start_row = hits$start_row[hits$name == "dep-code"][ok],
    start_column = hits$start_column[hits$name == "dep-code"][ok],
    start_byte = hits$start_byte[hits$name == "dep-code"][ok]
  )
}

parse_pkg_from_library_call <- function(fn, code) {
  expr <- parse(text= code, keep.source = FALSE)
  fun <- switch(fn,
    "library" = base::library,
    "require" = base::require,
    "loadNamespace" = base::loadNamespace,
    "requireNamespace" = base::requireNamespace
  )
  matched <- match.call(fun, expr, expand.dots = FALSE)

  pkg <- matched[["package"]]
  if (is.character(pkg) && length(pkg) == 1) {
    return(pkg)
  }

  if (fn %in% c("library", "require") && is.symbol(pkg) &&
      identical(matched[["character.only"]] %||% FALSE, FALSE)) {
    return(as.character(pkg))
  }

  NA_character_
}

# -------------------------------------------------------------------------

scan_path_deps_do_rmd <- function(code, path) {
  # TODO
}
