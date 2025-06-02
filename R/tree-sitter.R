ts_languages <- c(
  r = 0L,
  markdown = 1L,
  "markdown-inline" = 2L,
  yaml = 3L,
  toml = 4L
)

ts_detected_languages <- c(
  r = "r",
  md = "markdown",
  markdown = "markdown",
  rmd = "markdown",
  Rmarkdown = "markdown",
  yaml = "yaml",
  yml = "yaml",
  toml = "toml"
)

s_expr <- function(
  code,
  language = c("r", "markdown", "markdown-inline", "yaml", "toml"),
  ranges = NULL
) {
  language <- tolower(language)
  language <- ts_languages[match.arg(language)]
  if (is.character(code)) code <- charToRaw(paste(code, collapse = "\n"))
  call_with_cleanup(c_s_expr, code, language, ranges)
}

token_table <- function(
  file = NULL,
  language = NULL,
  ranges = NULL,
  text = NULL
) {
  if (is.null(text) + is.null(file) != 1) {
    stop(
      "Invalid arguments in `token_table()`: exactly one of ",
      "`file` and `text` must be given."
    )
  }
  if (is.null(text)) {
    text <- readBin(file, "raw", n = file.size(file))
    if (is.null(language)) {
      ext <- tolower(tools::file_ext(file))
      if (!ext %in% names(ts_detected_languages)) {
        stop(
          "Cannot detect language in `token_table(), need to specify",
          "it explicitly"
        )
      }
      language <- ts_detected_languages[ext]
    }
  } else if (is.null(language)) {
    stop("Invalid arguments in `token_table()`: need to specify `language`.")
  } else {
    language <- match.arg(tolower(language), names(ts_languages))
  }
  language <- ts_languages[language]
  if (is.character(text)) text <- charToRaw(paste(text, collapse = "\n"))
  tab <- call_with_cleanup(c_token_table, text, language, ranges)
  lvls <- seq_len(nrow(tab))
  tab$children <- I(unname(split(lvls, factor(tab$parent, levels = lvls))))
  attr(tab, "file") <- file
  tab
}

syntax_tree <- function(
  file = NULL,
  language = NULL,
  ranges = NULL,
  text = NULL
) {
  tokens <- token_table(file, language, ranges = ranges, text = text)

  type <- tokens$type
  fn <- attr(tokens, "file")
  if (cli::ansi_has_hyperlink_support() && !is.null(fn)) {
    type <- cli::style_hyperlink(
      type,
      sprintf(
        "file://%s:%d:%d",
        normalizePath(fn, mustWork = NA),
        tokens$start_row + 1L,
        tokens$start_column + 1
      )
    )
  }

  linum <- tokens$start_row + 1
  linum <- ifelse(duplicated(linum), "", as.character(linum))
  linum <- format(linum, justify = "right")
  # this is the spacer we need to put in for multi-line tokens
  nlspc <- paste0("\n\t", strrep(" ", nchar(linum[1])), "|")
  code <- ifelse(
    is.na(tokens$code),
    "",
    paste0(strrep(" ", tokens$start_column), tokens$code)
  )

  # we put in a \t, and later use it to align the lines vertically
  treetab <- data_frame(
    id = as.character(tokens$id),
    children = lapply(tokens$children, as.character),
    label = paste0(
      type,
      "\t",
      linum,
      "|",
      gsub("\n", nlspc, code, fixed = TRUE)
    )
  )
  tree <- cli::tree(treetab)

  # align lines vertically. the size of the alignment is measured
  # without the ANSI sequences, but then the substitution uses the
  # full ANSI string
  tabpos <- regexpr("\t", cli::ansi_strip(tree), fixed = TRUE)
  maxtab <- max(tabpos)
  tabpos2 <- regexpr("\t", tree, fixed = TRUE)
  regmatches(tree, tabpos2) <- strrep(" ", maxtab - tabpos + 4)

  tree
}

code_query <- function(
  code = NULL,
  query,
  file = NULL,
  language = c("r", "markdown", "markdown-inline", "yaml", "toml"),
  ranges = NULL
) {
  language <- tolower(language)
  language <- ts_languages[match.arg(language)]
  qlen <- nchar(query, type = "bytes") + 1L # + \n
  qbeg <- c(1L, cumsum(qlen))
  qnms <- names(query) %||% rep(NA_character_, length(query))
  query1 <- paste0(query, "\n", collapse = "")

  if (!is.null(code)) {
    if (is.character(code)) code <- charToRaw(paste(code, collapse = "\n"))
    res <- call_with_cleanup(c_code_query, code, query1, language, ranges)
  } else {
    res <- call_with_cleanup(c_code_query_path, file, query1, language, ranges)
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
      end_byte = viapply(res[[2]], "[[", 7L),
      start_row = viapply(res[[2]], "[[", 8L),
      start_column = viapply(res[[2]], "[[", 9L),
      end_row = viapply(res[[2]], "[[", 10L),
      end_column = viapply(res[[2]], "[[", 11L),
      name = vcapply(res[[2]], "[[", 4L),
      code = vcapply(res[[2]], "[[", 5L)
    )
  )
}
