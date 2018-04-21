
#' @importFrom glue glue_collapse backtick

format_items <- function(x) {
  paste0(
    glue_collapse(backtick(x), sep = ", ", last = " and ")
  )
}
