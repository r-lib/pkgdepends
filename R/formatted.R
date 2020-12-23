
add_format <- function(x, ...) {
  fmt <- format(x, ...)
  class(x) <- c("formatted", class(x))
  attr(x, "formatted") <- fmt
  x
}

#' @export

format.formatted <- function(x, ...) {
  attr(x, "formatted")
}

#' @export

print.formatted <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
