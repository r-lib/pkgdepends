
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
      private$data$start <- Sys.time()

      type <- match.arg(type)
      format <- if (type == "resolution") {
        paste0("  Crawling dependencies for :gtotal packages:",
               " [:gcount/:gtotal] :elapsedfull")
      } else {
        paste0("  Downloading packages [:count/:total] ",
               "[:strcbytes] :elapsedfull")
      }

      private$bar <- progress_bar$new(
        show_after = show_after, total = total, format = format, ...)
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
      data <- private$data
      data$gcount <- data$count + data$xcount
      data$gtotal <- data$total + data$xtotal
      private$bar$tick(0, tokens = data)
    },

    done = function() {
      if (!private$active) return()
      private$bar$terminate()
    },

    message = function(...) {
      if (!private$active) return()
      msg <- glue::glue_data(private$data, ...)
      if (private$bar$finished) {
        message(msg)
      } else{
        private$bar$message(msg)
      }
    },

    report = function() {
      if (!private$active) return()
      tck <- crayon::green(symbol$tick)
      if (!private$data$total) {
        self$message(tck,  " No downloads are needed")
      } else {
        data <- private$data
        dl <- data$count - data$failed - data$cached
        self$message(
          tck, " ", data$total, " packages",
          if (dl) paste0(", ", dl, " downloaded") else "",
          if (data$cached) paste0(", {cached} cached") else "",
          if (data$failed) paste0(", {failed} failed") else "",
          ", in ", pretty_dt(Sys.time() - data$start)
        )
      }
    }
  ),

  private = list(
    data = list(
      start = NULL,
      count = 0,    ## number of refs/files done
      total = 0,    ## total number of refs/files
      xcount = 0,   ## number of deps done, for resolution
      xtotal = 0,   ## total number of deps, for resolution
      gcount = 0,
      gtotal = 0,
      cbytes = 0,   ## number of bytes done (download)
      btotal = 0,   ## total number of bytes (download)
      cached = 0,   ## number of cached packages (download)
      bcached = 0,  ## cached bytes (download)
      failed = 0,   ## number of failed packages
      strcbytes = "",
      strbtotal = ""
    ),
    active = NULL,
    bar = NULL,
    template = NULL
  )
)
