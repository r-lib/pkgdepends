
#' @importFrom crayon inverse blue

logo <- function() {
  inverse(blue(" \U0001F13F  "))
}

cat_msg <- function(str) {
  cat(bold(str), sep = "\n")
}

#' @importFrom glue collapse backtick

format_items <- function(x) {
  paste0(
    collapse(backtick(x), sep = ", ", last = " and ")
  )
}
