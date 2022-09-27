
format_items <- function(x) {
  paste0(
    cli::ansi_collapse(paste0("`", x, "`"))
  )
}
