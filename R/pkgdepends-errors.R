
pkg_error <- function(..., .data = NULL, .class = NULL, .envir = parent.frame(),
                      call. = TRUE) {
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    cli::format_error(
      .envir = .envir,
      c(
        ...
      )
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(class)) class(cnd) <- c(.class, class(cnd))

  cnd
}

stop <- function(..., call. = TRUE, domain = NA) {
  .hide_from_trace <- TRUE
  args <- list(...)
  if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    throw(args[[1]])
  } else {
    throw(new_error(..., call. = call., domain = domain))
  }
}
