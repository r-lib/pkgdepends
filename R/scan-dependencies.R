code_query <- function(code, query) {
  if (is.character(code)) code <- charToRaw(code)
  res <- call_with_cleanup(c_code_query, code, query)
  list(
    patterns = data_frame(
      id = seq_along(res[[1]][[1]]),
      pattern = res[[1]][[1]],
      match_count = res[[1]][[2]]
    ),
    matched_captures = data_frame(
      pattern = viapply(res[[2]], "[[", 1L),
      match = viapply(res[[2]], "[[", 2L),
      capture = viapply(res[[2]], "[[", 3L),
      capture_start_byte = viapply(res[[2]], "[[", 6L),
      capture_name = vcapply(res[[2]], "[[", 4L),
      capture_code = vcapply(res[[2]], "[[", 5L)
    )
  )
}

s_expr <- function(code) {
  if (is.character(code)) code <- charToRaw(code)
  call_with_cleanup(c_s_expr, code)
}
