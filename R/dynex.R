
dynex <- function(code) {
  code <- sub("\\s+$", "", sub("^\\s+", "", code))
  code <- gsub("\n\n", "\n", code, fixed = TRUE)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  needs <- which(grepl("^#+ Needs[ :]", lines))
  cid <- cut(
    seq_along(lines),
    breaks = c(if (! 1 %in% needs) 0, needs, length(lines) + 1L),
    right = FALSE,
    include.lowest = TRUE)
  blocks <- tapply(lines, cid, c)

  drop_trailing_empty_lines <- function(x) {
    empty <- grepl("^\\s*$", x)
    wempty <- which(empty)
    drop <- wempty[wempty > max(which(!empty))]
    if (length(drop) > 0) x <- x[-drop]
    x
  }

  dynex_block <- function(str) {
    str <- drop_trailing_empty_lines(str)
    if (grepl("^#+ Needs[ :]", str[1])) {
      call <- sub("^#+ Needs[^:]*:", "", str[1])
      omit <- tryCatch(!isTRUE(eval(parse(text = call))), error = function(e) TRUE)
      if (omit) {
        str <- paste0("#x ", str)
      } else if (grepl("^#+ Needs: TRUE", str[1])) {
        str <- str[-1]
      }
    }
    paste(str, collapse = "\n")
  }

  paste(vapply(blocks, dynex_block, character(1)), collapse = "\n\n")
}
