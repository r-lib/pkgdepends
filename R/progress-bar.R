
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

      private$active <- getOption("pkg.progress.bar") %||% interactive()
      if (! private$active) return()

      type <- match.arg(type)
      format <- if (type == "resolution") {
        paste0("  Resolving   [:count/:total]   ",
               "deps: [:xcount/:xtotal] :elapsedfull")
      } else {
        paste0("  Downloading [:count/:total] ",
               "[:bytes/:btotal] :elapsedfull")
      }

      private$bar <- progress_bar$new(
        show_after = show_after, total = total, format = format, ...)
      private$bar$tick(0, tokens = private$data)
    },

    update = function(...) {
      if (!private$active) return()
      args <- list(...)
      if (is.null(names(args))) {
        key <- args[[1]]
        private$data[[key]] <- private$data[[key]] + args[[2]]
      } else {
        for (n in names(args)) {
          private$data[[n]] <- private$data[[n]] + args[[n]]
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
      message(glue::glue_data(private$data, ...))
    }
  ),

  private = list(
    data = list(
      count = 0,
      total = 0,
      xcount = 0,
      xtotal = 0,
      bytes = 0,
      btotal = 0
    ),
    active = NULL,
    bar = NULL,
    template = NULL
  )
)
