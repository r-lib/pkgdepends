
#' @importFrom zip unzip_process

make_unzip_process <- function(zipfile, exdir = ".", post_process = NULL) {
  up <- unzip_process()
  up$new(zipfile, exdir = exdir, post_process = post_process,
         stdout = "|", stderr = "|")
}
