
#' @importFrom tools md5sum file_ext

CRANMetadataCache <- R6Class(
  "CRANMetadataCache",

  public = list(
    initialize = function() {
      private$data <- new.env(parent = emptyenv())
      invisible(self)
    },

    get = function(path) {
      path <- normalizePath(path)
      md5 <- md5sum(path)
      if (! is.null(res <- private$data[[md5]])) {
        res
      } else {
        private$insert_file(path, md5)
      }
    }
  ),

  private = list(
    data = NULL,

    insert_file = function(path, md5) {
      ext <- file_ext(path)
      if (ext == "gz") {
        private$insert_file_gz(path, md5)
      } else if (ext == "rds") {
        private$insert_file_rds(path, md5)
      }
    },

    insert_file_gz = function(path, md5) {
      pkgs <- read.dcf.gz(path)
      private$data[[md5]] <- pkgs
      pkgs
    },

    insert_file_rds = function(path, md5) {
      obj <- format_archive_rds(readRDS(path))
      private$data[[md5]] <- obj
      obj
    }
  )
)
