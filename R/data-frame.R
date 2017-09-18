
check_data_frame_columns <- function(df, ...) {
  cols <- list(...)
  assert_that(all_named(cols))

  if (any(bad <- ! names(cols) %in% names(df))) {
    stop("Unknown column(s): ", paste(names(cols)[bad], collapse = ", "))
  }
}

find_in_data_frame <- function(df, ...) {
  cols <- list(...)
  idx <- seq_len(nrow(df))
  for (i in seq_along(cols)) {
    if (length(idx) == 0) break
    if (is.null(cols[[i]])) next
    n <- names(cols)[i]
    idx <- idx[df[[n]][idx] %in% cols[[i]]]
  }

  idx
}

append_to_data_frame <- function(df, ...) {
  cols <- list(...)
  assert_that(all_named(cols))

  if (any(bad <- ! names(cols) %in% names(df))) {
    stop("Unknown column(s): ", paste(names(cols)[bad], collapse = ", "))
  }

  cols <- ifelse(names(df) %in% names(cols), cols[names(df)], NA_integer_)
  res <- rbind(df, cols, stringsAsFactors = FALSE)
  names(res) <- names(df)
  res
}

delete_from_data_frame <- function(df, ...) {
  idx <- find_in_data_frame(df, ...)
  if (length(idx)) df[-idx, ] else df
}
