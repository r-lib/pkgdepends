
#' @importFrom glue collapse backtick

format_items <- function(x) {
  paste0(
    collapse(backtick(x), sep = ", ", last = " and ")
  )
}
