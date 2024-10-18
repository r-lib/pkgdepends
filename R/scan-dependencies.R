code_query <- function(code = NULL, query, file = NULL) {
  qlen <- nchar(query, type = "bytes") + 1L # + \n
  qbeg <- c(1L, cumsum(qlen))
  qnms <- names(query) %||% rep(NA_character_, length(query))
  query1 <- paste0(query, "\n", collapse = "")

  if (!is.null(code)) {
    if (is.character(code)) code <- charToRaw(paste(code, collapse = "\n"))
    res <- call_with_cleanup(c_code_query, code, query1)
  } else {
    res <- call_with_cleanup(c_code_query_path, file, query1)
  }

  qorig <- as.integer(cut(res[[1]][[3]], breaks = qbeg, include.lowest = TRUE))

  list(
    patterns = data_frame(
      id = seq_along(res[[1]][[1]]),
      name = qnms[qorig],
      pattern = res[[1]][[1]],
      match_count = res[[1]][[2]]
    ),
    matched_captures = data_frame(
      id = viapply(res[[2]], "[[", 3L),
      pattern = viapply(res[[2]], "[[", 1L),
      match = viapply(res[[2]], "[[", 2L),
      start_byte = viapply(res[[2]], "[[", 6L),
      start_row = viapply(res[[2]], "[[", 7L),
      start_column = viapply(res[[2]], "[[", 8L),
      name = vcapply(res[[2]], "[[", 4L),
      code = vcapply(res[[2]], "[[", 5L)
    )
  )
}

s_expr <- function(code) {
  if (is.character(code)) code <- charToRaw(paste(code, collapse = "\n"))
  call_with_cleanup(c_s_expr, code)
}

# match any library() and require calls
# we use these as fallbacks. If a call is not identified some other way
# we parse it with R and match the call.
q_library_0 <- function() {
  '((call function: (identifier) @fn-name) @dep-code
    (#any-of? @fn-name "library" "require")
  )'
}

# library(foo)
# - function call is library or require
# - the only argument (. argument .) is not named (. value:)
# - the only argument is a symbol or a string
# - this should catch most of the usage, the rest we check with
#   `match.call()`
q_library_1 <- function() {
  '(call function: (identifier) @fn-name
    arguments: (arguments . argument: (argument . value: [
      (identifier) @pkg-name
      (string (string_content) @pkg-name)
    ]) . )
    (#any-of? @fn-name "library" "require")
  ) @dep-code'
}

q_colon <- function() {
  '(namespace_operator lhs: (identifier) @pkg-name) @dep-code'
}

q_deps <- function() {
  c(
    q_library_0 = q_library_0(),
    q_library_1 = q_library_1(),
    q_colon = q_colon()
  )
}

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
