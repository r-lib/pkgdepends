
#' @importFrom withr with_dir

unzip <- function(zipfile) {
  with_dir(
    dirname(zipfile),
    utils::unzip(zipfile, exdir = ".", unzip = "internal")
  )

  files <- utils::unzip(zipfile, list = TRUE, unzip = "internal")[,1]

  root_files <- grep("^[^/]+/?$", files, value = TRUE)

  sub("/$", "", root_files)
}

zip_list <- function(zipfile) {
  utils::unzip(zipfile, list = TRUE, unzip = "internal")[,1]
}
