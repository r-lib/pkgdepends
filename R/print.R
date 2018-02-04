
#' @importFrom glue collapse backtick

format_items <- function(x) {
  paste0(
    collapse(backtick(x), sep = ", ", last = " and ")
  )
}

#' The pkgdepends theme
#'
#' We need to export it, because it is 
#' 
#' @export

pkg_theme <- function() {
  list(
    "div.pkgx .alert-success" = list(color = "darkgrey"),
    "div.pkgx .alert-info" = list(color = "darkgrey"),
    "div.pkgx .alert-start" = list(
      before = paste0(symbol$arrow_right, " ")
    ),
    "div.pkgx span.pkg" = list(
      "font-weight" = "bold",
      color = "blue"
    ),
    "div.pkgx span.version" = list(
      color = "blue"
    )
  )
}

cat_msg <- function(str) {
  cat(bold(str), sep = "\n")
}
