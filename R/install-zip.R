
#' @importFrom zip unzip_process

make_unzip_process <- function(zipfile, exdir = ".",
                               post_process = NULL, stdout = "|",
                               stderr = "|", ...) {
  up <- unzip_process()
  up$new(zipfile, exdir = exdir, post_process = post_process,
         stdout = stdout, stderr = stderr, ...)
}
