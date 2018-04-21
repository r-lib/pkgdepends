
#' @export

print.remotes_resolution <- function(x, ...) {
  cat(format(x, ...))
}

#' @importFrom prettyunits pretty_dt
#' @importFrom crayon bgBlue green blue white bold col_nchar
#' @importFrom cli symbol
#' @export

format.remotes_resolution <- function(x, ...) {

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  meta <- attr(x, "metadata")

  direct <- unique(x$ref[x$direct])
  dt <- pretty_dt(meta$resolution_end - meta$resolution_start)
  head <- glue(
    "PKG RESOLUTION, {length(direct)} refs, resolved in {dt} ")
  width <- getOption("width") - col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(symbol$line, max(width, 0)))
  push(blue(bold(head)), sep = "\n")

  push(format_refs(x, x$direct, header = NULL))
  push(format_refs(x, (! x$direct), header = "Dependencies", by_type = TRUE))
  push(format_failed_refs(x))

  paste0(result, collapse = "")
}

get_failed_refs <- function(res) {
  failed <- tapply(res$status, res$ref, function(x) all(x != "OK"))
  names(which(failed))
}

#' @importFrom crayon red

format_refs <- function(res, which, header, by_type = FALSE,
                        mark_failed = TRUE) {
  if (!length(res$ref[which])) return()

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  if (!is.null(header)) push(blue(bold(paste0(header, ":"))), sep = "\n")

  mark <- function(wh, short = FALSE) {
    ref <- ref2 <- sort(unique(res$ref[wh]))
    if (short) ref2 <- basename(ref)
    if (mark_failed) {
      failed_ref <- get_failed_refs(res[wh,])
      ref2 <- ifelse(ref %in% failed_ref, bold(red(ref2)), ref2)
    }
    ref2
  }

  if (by_type) {
    for (t in sort(unique(res$type[which]))) {
      push(blue(paste0("  ", t, ":")), sep = "\n")
      which2 <- which & res$type == t
      push(comma_wrap(mark(which2, short = t == "installed"), indent = 4),
           sep = "\n")
    }

  } else {
    push(comma_wrap(mark(which)), sep = "\n")
  }

  paste0(result, collapse = "")
}

format_failed_refs <- function(res) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  failed <- get_failed_refs(res)
  if (length(failed) > 0) push(bold(red("Errors:")), sep = "\n")
  for (f in failed) push(format_failed_ref(res, f))

  paste0(result, collapse = "")
}

format_failed_ref <- function(res, failed_ref) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  push("  ", failed_ref, ": ")
  wh <- which(failed_ref == res$ref)
  errs <- unique(vcapply(res$error[wh], conditionMessage))
  push(paste(errs, collapse = "\n    "), sep = "\n")

  paste0(result, collapse = "")
}
