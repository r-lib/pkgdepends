
#' @importFrom prettyunits pretty_dt
#' @importFrom crayon bgBlue green blue white bold col_nchar
#' @importFrom cli symbol

print.remotes_resolution <- function(x, ...) {
  meta <- attr(x, "metadata")

  direct <- unique(x$ref[x$direct])
  dt <- pretty_dt(meta$resolution_end - meta$resolution_start)
  head <- glue(
    "PKG RESOLUTION, {length(direct)} refs, resolved in {dt} ")
  width <- getOption("width") - col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(symbol$line, max(width, 0)))
  cat(blue(bold(head)), sep = "\n")

  print_refs(x, x$direct, header = NULL)

  print_refs(x, (! x$direct), header = "Dependencies", by_type = TRUE)

  print_failed_refs(x)

  invisible(x)
}

get_failed_refs <- function(res) {
  failed <- tapply(res$status, res$ref, function(x) all(x != "OK"))
  names(which(failed))
}

#' @importFrom crayon red

print_refs <- function(res, which, header, by_type = FALSE,
                       mark_failed = TRUE) {
  if (!length(res$ref[which])) return()

  if (!is.null(header)) cat(blue(bold(paste0(header, ":"))), sep = "\n")

  mark <- function(wh, short = FALSE) {
    ref <- ref2 <- sort(unique(res$ref[wh]))
    if (short) ref2 <- basename(ref)
    if (mark_failed) {
      failed_ref <- get_failed_refs(res[wh,])
      ref <- ifelse(ref %in% failed_ref, bold(red(ref2)), ref2)
    }
    ref2
  }

  if (by_type) {
    for (t in sort(unique(res$type[which]))) {
      cat(blue(paste0("  ", t, ":")), sep = "\n")
      which2 <- which & res$type == t
      cat(comma_wrap(mark(which2, short = t == "installed"), indent = 4),
          sep = "\n")
    }

  } else {
    cat(comma_wrap(mark(which)), sep = "\n")
  }
}

print_failed_refs <- function(res) {
  failed <- get_failed_refs(res)
  if (length(failed) > 0) cat(bold(red("Errors:")), sep = "\n")
  for (f in failed) print_failed_ref(res, f)
}

print_failed_ref <- function(res, failed_ref) {
  cat0("  ", failed_ref, ": ")
  wh <- which(failed_ref == res$ref)
  errs <- unique(vcapply(
    res$resolution[wh],
    function(x) get_error_message(x) %||% "Unknown error"
  ))
  cat(paste(errs, collapse = "\n    "), sep = "\n")
}
