code_query <- function(code, query) {
  if (is.character(code)) code <- charToRaw(code)
  res <- call_with_cleanup(c_code_query, code, query)
  data_frame(
    pattern = viapply(res, "[[", 1L),
    match = viapply(res, "[[", 2L),
    capture = viapply(res, "[[", 3L),
    capture_start_byte = viapply(res, "[[", 6L),
    capture_name = vcapply(res, "[[", 4L),
    capture_code = vcapply(res, "[[", 5L)
  )
}
