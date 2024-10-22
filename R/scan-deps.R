#' Scan R code for dependent packages
#'
#' @keywords internal

scan_deps <- function(path = ".") {
  path <- find_project_root(path)
  paths <- dir(path, pattern = "[.](R|r|Rmd|rmd)$", recursive = TRUE)
  full_paths <- normalizePath(file.path(path, paths))
  deps_list <- lapply(full_paths, scan_path_deps)
  deps <- do.call("rbind", c(list(scan_path_deps_empty()), deps_list))
  # write back the relative paths
  deps$path <- paths[match(deps$path, full_paths)]
  deps$type <- get_dep_type_from_path(deps$path)
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

re_r_dep <- paste0(collapse = "|", c(
  "library", "require", "loadNamespace",
  "::",
  "setClass", "setGeneric",
  "pkg_attach",
  "p_load"
))

scan_path_deps <- function(path) {
  code <- readBin(path, "raw", file.size(path))

  # check if already known, set path
  hash <- cli::hash_raw_xxhash(code)
  cache <- get_deps_cache_path(hash)
  if (file.exists(cache)) {
    deps <- readRDS(cache)
    if (!is.null(deps) && nrow(deps) > 0) {
      deps$path <- path
      deps$type <- get_dep_type_from_path(path)
    }
    return(deps)
  }

  # scan it if it is worth it, based on a quick check
  has_deps <- length(grepRaw(re_r_dep, code)) > 0
  deps <- if (has_deps) scan_path_deps_do(code, path)

  # save it to the cache, but anonimize it first. If no deps, save NULL
  deps_no_path <- deps
  if (!is.null(deps_no_path) && nrow(deps_no_path) > 0) {
    deps_no_path$path <- ""
    deps_no_path$type <- NA_character_
  }
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(deps_no_path, cache)

  deps
}

scan_path_deps_empty <- function() {
  data_frame(
    path = character(),
    package = character(),
    type = character(),
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
  mct <- hits$matched_captures

  # q_library_0 hits are generic ones, only use them if they are not hit
  gen_pat <- hits$patterns$id[hits$patterns$name == "q_library_0"]
  gen_hits <- mct[mct$pattern %in% gen_pat, ]

  # for these patterns we need to work from the function names
  fn_patterns <- "methods"
  fn_pat <- hits$patterns$id[hits$patterns$name %in% fn_patterns]
  fn_hits <- mct[mct$pattern %in% fn_pat, ]

  pkg_hits <- mct[! mct$pattern %in% c(gen_pat, fn_pat), ]
  rbind(
    if (nrow(pkg_hits) > 0) scan_path_deps_do_pkg_hits(pkg_hits, path),
    if (nrow(fn_hits) > 0) scan_path_deps_do_fn_hits(fn_hits, path),
    if (nrow(gen_hits) > 0) scan_path_deps_do_gen_hits(gen_hits, path)
  )
}

scan_path_deps_do_pkg_hits <- function(hits, path) {
  data_frame(
    path = path,
    package = hits$code[hits$name == "pkg-name"],
    type = get_dep_type_from_path(path),
    code = hits$code[hits$name == "dep-code"],
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_path_deps_do_fn_hits <- function(hits, path) {
  fn_pkg_map <- c(setClass = "methods", setGeneric = "methods")
  fn_names <- hits$code[hits$name == "fn-name"]
  data_frame(
    path = path,
    package = fn_pkg_map[fn_names],
    type = get_dep_type_from_path(path),
    code = hits$code[hits$name == "dep-code"],
    start_row = hits$start_row[hits$name == "dep-code"],
    start_column = hits$start_column[hits$name == "dep-code"],
    start_byte = hits$start_byte[hits$name == "dep-code"]
  )
}

scan_path_deps_do_gen_hits <- function(hits, path) {
  code <- hits$code[hits$name == "dep-code"]
  fn <- hits$code[hits$name == "fn-name"]
  pkgs <- lapply(seq_along(code), function(i) {
    safe_parse_pkg_from_call(fn[i], code[i])
  })
  pkgs_count <- lengths(pkgs)
  data_frame(
    path = path,
    package = unlist(pkgs),
    type = get_dep_type_from_path(path),
    code = rep(code, pkgs_count),
    start_row = rep(hits$start_row[hits$name == "dep-code"], pkgs_count),
    start_column = rep(hits$start_column[hits$name == "dep-code"], pkgs_count),
    start_byte = rep(hits$start_byte[hits$name == "dep-code"], pkgs_count)
  )
}

fake_xfun_pkg_attach <- function(..., install, message) { }
fake_xfun_pkg_attach2 <- function(...) { }
fake_pacman_p_load <- function(..., char, install, update, character.only) { }

safe_parse_pkg_from_call <- function(fn, code) {
  tryCatch(
    parse_pkg_from_call(fn, code),
    error = function(...) NULL
  )
}

parse_pkg_from_call <- function(fn, code) {
  expr <- parse(text = code, keep.source = FALSE)
  fun <- switch(fn,
    "library" = base::library,
    "require" = base::require,
    "loadNamespace" = base::loadNamespace,
    "requireNamespace" = base::requireNamespace,
    "pkg_attach" = fake_xfun_pkg_attach,
    "pkg_attach2" = fake_xfun_pkg_attach2,
    "p_load" = fake_pacman_p_load,
  )
  matched <- match.call(fun, expr, expand.dots = FALSE)
  switch(fn,
    "library" = , "require" =
      parse_pkg_from_call_library(matched),
    "loadNamespace" = , "requireNamespace" =
      parse_pkg_from_call_loadNamespace(matched),
    "pkg_attache" = , "pkg_attach2" =
      parse_pkg_from_call_xfun(matched),
    "p_load" =
      parse_pkg_from_call_pacman(matched)
  )
}

parse_pkg_from_call_library <- function(matched) {
  pkg <- matched[["package"]]
  if (is.character(pkg) && length(pkg) == 1) {
    return(pkg)
  }
  if (is.symbol(pkg) &&
      identical(matched[["character.only"]] %||% FALSE, FALSE)) {
    return(as.character(pkg))
  }
  NULL
}

parse_pkg_from_call_loadNamespace <- function(matched) {
  pkg <- matched[["package"]]
  if (is.character(pkg) && length(pkg) == 1) {
    return(pkg)
  }
  NULL
}

parse_pkg_from_call_xfun <- function(matched) {
  pkgs <- unlist(lapply(
    matched[["..."]],
    function(x) if (is.character(x)) x
  ))
  if (length(pkgs) > 0) return(pkgs)
  NULL
}

parse_pkg_from_call_pacman <- function(matched) {
  # list of characters and symbols
  pkgs <- as.list(matched[["..."]])

  # character vector or scalar
  char <- matched[["char"]]
  if (char[[1]] == quote(c)) {
    pkgs <- c(pkgs, as.list(char[-1]))
  } else if (is.character(char)) {
    pkgs <- c(pkgs, as.list(char))
  }
  if (matched[["character.only"]] %||% FALSE) {
    pkgs <- pkgs[vlapply(pkgs, function(x) is.character(x))]
  } else {
    pkgs <- pkgs[vlapply(pkgs, function(x) is.symbol(x) || is.character(x))]
  }
  pkgs <- vcapply(pkgs, as.character)
  if (length(pkgs) > 0) return(pkgs)
  NULL
}

# -------------------------------------------------------------------------

scan_path_deps_do_rmd <- function(code, path) {
  hits <- code_query(code, language = "markdown", query = q_deps_rmd())
  inl_pat <- hits$patterns$id[hits$patterns$name == "inline"]
  inl_hits <- hits$matched_captures[
    hits$matched_captures$pattern %in% inl_pat, ]
  blk_hits <- hits$matched_captures[
    ! hits$matched_captures$pattern %in% inl_pat, ]
  rbind(
    if (nrow(inl_hits)) scan_path_deps_do_inline_hits(code, inl_hits, path),
    if (nrow(blk_hits)) scan_path_deps_do_block_hits(code, blk_hits, path)
  )
}

range_cols <- c(
  "start_row", "start_column", "end_row", "end_column",
  "start_byte", "end_byte"
)

scan_path_deps_do_inline_hits <- function(code, inl_hits, path) {
  wcnd <- which(inl_hits$name == "inline")
  wcnd <- wcnd[grepl("`", inl_hits$code[wcnd], fixed = TRUE)]
  wcnd <- wcnd[grepl(re_r_dep, inl_hits$code[wcnd])]
  if (length(wcnd) == 0) {
    return(NULL)
  }

  inl_ranges <- inl_hits[wcnd, range_cols]
  r_hits <- code_query(
    code,
    language = "markdown-inline",
    ranges = inl_ranges,
    query = q_deps_rmd_inline()
  )
  cpt <- r_hits$matched_captures
  pre_drop <- nchar(cpt$code[cpt$name == "csd1"])
  post_drop <- nchar(cpt$code[cpt$name == "csd2"])
  r_code <- omit_pre_post(cpt$code[cpt$name == "code"], pre_drop, post_drop)
  wcnd2 <- substr(r_code, 1, 2) == "r " & grepl(re_r_dep, r_code)
  if (!any(wcnd2)) {
    return(NULL)
  }
  # need to adjust the ranges for the _ASCII_ (!) delimiters
  r_ranges <- cpt[cpt$name == "code", ][wcnd2, range_cols]
  r_ranges$start_byte <- r_ranges$start_byte + pre_drop[wcnd2] + 2L   # 'r '
  r_ranges$start_column <- r_ranges$start_column + pre_drop[wcnd2] + 2L
  r_ranges$end_byte <- r_ranges$end_byte - post_drop[wcnd2]
  scan_path_deps_do_r(code, path = path, ranges = r_ranges)
}

scan_path_deps_do_block_hits <- function(code, blk_hits, path) {
  wcnd <- which(blk_hits$name == "content")
  wcnd <- wcnd[grepl(re_r_dep, blk_hits$code[wcnd])]
  if (length(wcnd) == 0) {
    return(NULL)
  }

  r_ranges <- blk_hits[wcnd, range_cols]
  scan_path_deps_do_r(code, path = path, ranges = r_ranges)
}
