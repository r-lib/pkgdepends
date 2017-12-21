
is_verbose <- function() {
  getOption("pkg.progress.bar") %||% interactive()
}

remotes__with_progress_bar <- function(self, private, args, expr) {
  private$progress_bar <- do.call(pkg_progress_bar$new, as.list(args))
  on.exit(private$progress_bar$done())
  expr
}

#' @importFrom progress progress_bar

pkg_progress_bar <- R6Class(
  "pkg_progress_bar",

  public = list(
    initialize = function(type = c("resolution", "download"),
      show_after = 0, total = 1e10, ...) {

      private$active <- is_verbose()
      if (! private$active) return()

      private$data$total <- total

      type <- match.arg(type)
      format <- if (type == "resolution") {
        paste0("  Resolving   [:count/:total]   ",
               "deps: [:xcount/:xtotal] :elapsedfull")
      } else {
        paste0("  Downloading [:count/:total] ",
               "[:strcbytes/:strbtotal] :elapsedfull")
      }

      private$bar <- progress_bar$new(
        show_after = show_after, total = total, format = format, ...)
      private$bar$tick(0, tokens = private$data)
    },

    update = function(...) {
      if (!private$active) return()
      args <- list(...)
      if (is.null(names(args))) {
        args <- structure(list(args[[2]]), names = args[[1]])
      }
      for (n in names(args)) {
        private$data[[n]] <- private$data[[n]] + args[[n]]
        if (n == "cbytes") {
          private$data$strcbytes <- pretty_bytes(private$data$cbytes)
        }
        if (n == "btotal") {
          private$data$strbtotal <- pretty_bytes(private$data$btotal)
        }
      }
      private$bar$tick(0, tokens = private$data)
    },

    done = function() {
      if (!private$active) return()
      private$bar$terminate()
    },

    ## This has to come after the PB is finished
    message = function(...) {
      if (!private$active) return()
      message(glue::glue_data(private$data, ...))
    }
  ),

  private = list(
    data = list(
      count = 0,    ## number of refs/files done
      total = 0,    ## total number of refs/files
      xcount = 0,   ## number of deps done, for resolution
      xtotal = 0,   ## total number of deps, for resolution
      cbytes = 0,   ## number of bytes done (download)
      btotal = 0,   ## total number of bytes (download)
      cached = 0,   ## number of cached packages (download)
      bcached = 0,  ## cached bytes (download) (unused)
      failed = 0,   ## number of failed packages
      strcbytes = "",
      strbtotal = ""
    ),
    active = NULL,
    bar = NULL,
    template = NULL
  )
)
