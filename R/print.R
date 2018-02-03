
#' @importFrom glue collapse backtick

format_items <- function(x) {
  paste0(
    collapse(backtick(x), sep = ", ", last = " and ")
  )
}

pkg_theme <- function() {
  list(".alert-start" = list(
    before = paste0(symbol$arrow_right, " ")
  ))
}

cat_msg <- function(str) {
  cat(bold(str), sep = "\n")
}
