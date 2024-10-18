scan_path_deps <- function(path) {
  code <- readBin(path, "raw", file.size(path))
  has_library <- length(grepRaw("library", code, fixed = TRUE)) > 0
  has_require <- length(grepRaw("require", code, fixed = TRUE)) > 0
  has_colon <- length(grepRaw("::", code, fixed = TRUE)) > 0

  deps <- if (has_library || has_require || has_colon) {
    scan_path_deps_do(code, path)
  } else {
    scan_path_deps_empty()
  }

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
  hits <- code_query(code, q_deps())
  # q_library_0 hits are generic ones, only use them if they are not hit
  gen_pat <- hits$patterns$id[hits$patterns$name == "q_library_0"]
  gen_hits <- hits$matched_captures[hits$matched_captures$pattern == gen_pat, ]
  ok_hits <- hits$matched_captures[hits$matched_captures$pattern != gen_pat, ]
  gen_hits <- gen_hits[! gen_hits$start_byte %in% ok_hits$start_byte, ]
  rbind(
    scan_path_deps_empty(),
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
    "require" = base::require
  )
  matched <- match.call(fun, expr, expand.dots = FALSE)

  pkg <- matched[["package"]]
  if (is.character(pkg) && length(pkg) == 1) {
    return(pkg)
  }

  if (is.symbol(pkg) &&
      identical(matched[["character.only"]] %||% FALSE, FALSE)) {
    return(as.character(pkg))
  }

  NA_character_
}

scan_deps <- function(path = ".") {
  paths <- dir(path, pattern = "[.]R$", recursive = TRUE)
  do.call("rbind", lapply(paths, scan_path_deps))
}
