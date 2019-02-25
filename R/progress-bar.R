
remotes__with_progress_bar <- function(self, private, args, expr) {
  private$progress_bar <- do.call(pkg_progress_bar()$new, as.list(args))
  on.exit(private$progress_bar$done())
  expr
}

#' @importFrom glue glue_data
#' @importFrom prettyunits pretty_dt
#' @importFrom cliapp cli_progress_bar

pkg_progress_bar <- function() {
  R6Class(
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

        private$bar <- cli_progress_bar(
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

      report = function() {
        if (!private$active) return()
        if (!private$data$total) {
          cli_alert_success("No downloads are needed")
        } else {
          data <- private$data
          dl <- data$count - data$failed - data$cached
          self$alert_success(paste0(
            data$total, " packages",
            if (dl) paste0(", ", dl, " downloaded") else "",
            if (data$cached) paste0(", {cached} cached") else "",
            if (data$failed) paste0(", {failed} failed") else "",
            ", in ", pretty_dt(Sys.time() - data$start)
          ))
        }
      },

      alert = function(text, ...) {
        if (!private$active) return()
        text <- glue_data(private$data, text, .envir = parent.frame())
        cli_alert(text, ...)
      },

      alert_success = function(text, ...) {
        if (!private$active) return()
        text <- glue_data(private$data, text, .envir = parent.frame())
        cli_alert_success(text, ...)
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
}
