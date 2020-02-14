
#' The download progress bar
#'
#' This is a short summary of the design of the download progress bar.
#'
#' ## Package sizes
#'
#' One difficulty here is that we don't always know the sizes of the files
#' we are downloading. We basically have three cases for downloads:
#' * For some, we know the sizes from the extra CRAN metadata. This is
#'   usually correct, unless CRAN has changed the file on the web server.
#'   If we get a size from the web server, then we'll use that.
#' * For some, we do not know the size beforehand, but the web server
#'   sends us the correct size, together with the first chunk of data.
#'   This is typically for CRAN packages that we don't have metadata
#'   about, Bioconductor packages, or GitHub packages for which GitHub
#'   already knows the size.
#' * For some, we do not know the size, and the web server does not tell
#'   us the size, either. This is typically for GH packages that GH is
#'   creating on the fly.
#'
#' So ideally we would have a different progress bar when the total
#' size if known.
#'
#' ## Possible information to show:
#'
#' * Visual progress bar
#' * Number of packages: done / in-progress / total
#' * Number of bytes: done / total (if known)
#' * ETA
#' * Download rate, i.e. x kB/s
#' * Elapsed time
#' * Percent of packages done
#' * Percent of bytes done
#' * Spinner to show that we are still working, although updating ETA is
#'   good as well...
#' * Events, e.g. got a package, downloading, or starting...
#'
#' ## All package sizes are known
#'
#' ```
#' 230 kB/s (#####      ) 45% | 14/56 pkgs | ETA ~34s | Got dplyr
#' ```
#'
#' * Rate is informative, serves as an "alive" indicator as well.
#' * Bar is visual.
#' * Percent is the best quantitative measure.
#' * Done/total packages is informative, not so much for the download
#'   but the whole installation.
#' * ETA is informative.
#' * Event is nice, and it fills the space...
#' * We don't show current/total bytes, shoulwd we? Quite wide...
#'
#' ## Some package sizes are unknown
#'
#' ```
#' 230 kB/s (#####      ) 45% | 14/56 pkgs | Got dplyr
#' ```
#'
#' * Percent is for the packages, not bytes.
#' * We can't really show ETA.
#'
#' ## Events
#'
#' It is a question how to decide what to show in the event part.
#' The goal is to show interesting events first. In the order or priority:
#' * `Connecting...` is shown initially, when we have no data.
#' * `Check your connection...` if the current rate is zero and
#'   the downloads have started more than 4 seconds ago.
#' * `Got 3 pkgs: foo, bar and foobar`
#' * `Getting 3 pkgs: foo, bar and foobar`
#'
#' We only change the event message every 1/2 sec.
#'
#' ## Rate (download "speed")
#'
#' To calculate the rate we need to record when the various data chunks
#' have arrived. We only do this with a resolution of 1s. We create an
#' environment called chunks, and for every second since the downloads have
#' started we record the number of bytes that arrived in that second.
#'
#' When calculating the rate, we take the data for the last 4 seconds,
#' and normalize it to per second, while taking into account that the
#' data for the last (current) second is incomplete.
#'
#' In the first 3 seconds we use all the available data.
#'
#' @name pkgdepends-download-progress-bar
#' @keywords internal
NULL

#' @importFrom cli get_spinner cli_status

pkgplan__create_progress_bar <- function(what) {
  bar <- new.env(parent = emptyenv())

  whatx <- filter_needed_downloads(what)

  bar$status <- cli_status(
    "{.alert-info About to download {nrow(whatx)} package{?s}}",
    .auto_close = FALSE
  )

  bar$chars <- progress_chars()

  bar$what <- what[, c("type", "filesize", "package", "cache_status")]
  bar$what$idx <- seq_len(nrow(what))
  bar$what$current <- 0L                # We got this many bytes
  bar$what$event <- "todo"              # 'todo', "got", "done", "reported"
  bar$what$event_at <- Sys.time()[NA]   # time stamp of last event change

  bar$chunks <- new.env(parent = emptyenv())
  bar$start_at <- Sys.time()

  bar$timer <- new_async_timer(
    1/10,
    function() pkgplan__show_progress_bar(bar)
  )
  bar$timer$listen_on("error", function(e) { stop(e) })

  bar
}

#' Update the progress bar data
#'
#' This is triggered by libcurl.
#'
#' @param bar The progress bar.
#' @param idx The row index in the download data table.
#' @param data Data about this row, either `"done"`, meaning that the
#'   download has completed, or a list with entries `current` and `total`,
#'   measured in the number of bytes. If `total` is unknown, then it is
#'   set to zero.
#'
#' @keywords internal

pkgplan__update_progress_bar <- function(bar, idx, data) {
  time <- Sys.time()
  sec <- as.character(floor(as.double(
    time - bar$start_at,
    units = "secs"
  )))

  bar$what$event[idx] <- "got"
  bar$what$event_at[idx] <- time

  if (identical(data, "done")) {
    bar$what$event[idx] <- "done"
    if (!is.na(bar$what$filesize[idx])) {
      bar$chunks[[sec]] <- (bar$chunks[[sec]] %||% 0) -
        bar$what$current[idx] + bar$what$filesize[idx]
      bar$what$current[idx] <- bar$what$filesize[idx]
    }

  } else {
    ## TODO: redirects!
    bar$chunks[[sec]] <- (bar$chunks[[sec]] %||% 0) -
      bar$what$current[idx] + data$current
    bar$what$current[idx] <- data$current
    if (data$total > 0) {
      bar$what$filesize[idx] <- data$total
    }
    if (data$total > 0 && data$total == data$current) {
      bar$what$event[idx] <- "done"
    }
  }
}

#' Show / update the progress bar
#'
#' @param bar The progress bar object.
#'
#' @keywords internal
#' @importFrom glue glue_collapse
#' @importFrom prettyunits pretty_bytes pretty_dt

pkgplan__show_progress_bar <- function(bar) {

  parts <- calculate_progress_parts(bar)

  # Ready to update. We can't use the package emoji because its
  # width is not calculated properly
  str <- c(
    "\u00a0{parts$rate} {parts$line}{parts$percent} ",
    "| {parts$pkg_done}/{parts$pkg_total} pkg{?s} ",
    if (!is.na(parts$bytes_total)) "| ETA {parts$eta} ",
    "| {parts$msg}"
  )
  cli_status_update(bar$status, str)
}

filter_needed_downloads <- function(what) {
  what[(! what$type %in% c("installed", "deps")) &
       what$cache_status == "miss", ]
}

calculate_progress_parts <- function(bar) {

  parts <- list()

  sp <- function(x) gsub(" ", "\u00a0", x)

  # We filter these here, instead at the beginning, because otherwise
  # the indices would not work in the update function
  whatx <- filter_needed_downloads(bar$what)

  # Simple numbers
  parts$pkg_done <- sum(whatx$event %in% c("done", "reported"))
  parts$pkg_total <- nrow(whatx)
  pkg_percent <- parts$pkg_done / parts$pkg_total
  bytes_done <- sum(whatx$current, na.rm = TRUE)
  bytes_total <- sum(whatx$filesize)           # could be NA
  parts$bytes_total <- bytes_total
  bytes_percent <- bytes_done / bytes_total # could be NA
  percent <- if (!is.na(bytes_percent)) bytes_percent else pkg_percent
  if (round(percent * 100) == 100 && percent < 1) percent <- 0.99
  parts$percent <- sp(format(
    paste0(round(100 * percent), "%"),
    width = 4,
    justify = "right"
  ))

  # Rate, see above how this works
  time_at <- as.double(Sys.time() - bar$start_at, units = "secs")
  time_at_s <- as.integer(floor(time_at))
  labels <- as.character(seq(time_at_s, time_at_s - 3L, by = -1L))
  data <- unlist(mget(labels, envir = bar$chunks, ifnotfound = 0L))
  fact <- time_at - max(time_at_s - 3, 0)
  rate <- sum(data) / fact
  if (rate == 0) {
    parts$rate <- strrep("\u00a0", 8)
  } else {
    parts$rate <- sp(paste0(pretty_bytes(rate, style = "6"), "/s"))
  }

  # Message
  if (bytes_done == 0) {
    msg <- "Connecting"
  } else if (rate == 0 && time_at > 4) {
    msg <- "Check your connection"
  } else if (any(whatx$event == "done")) {
    idx <- bar$what$idx[bar$what$event == "done"]
    if (length(idx) == 1) {
      msg <- paste0("Got ", bar$what$package[idx])
    } else {
      msg <- paste0(
        "Got ", length(idx), " pkgs: ",
        glue_collapse(bar$what$package[idx], sep = ", ", last = " and ")
      )
    }
    bar$what$event[idx] <- "reported"
  } else if (any(whatx$event == "got")) {
    idx <- bar$what$idx[bar$what$event == "got"]
    if (length(idx) == 1) {
      msg <- paste0("Getting ", bar$what$package[idx])
    } else {
      msg <- paste0(
        "Getting ", length(idx), " pkgs: ",
        glue_collapse(bar$what$package[idx], sep = ", ", last = " and ")
      )
    }
  } else {
    msg <- ""
  }
  parts$msg <- msg

  # Line
  parts$line <- make_bar(bar$chars, percent, width = 15)

  # ETA
  if (!is.na(bytes_total)) {
    if (rate == 0) {
      parts$eta <- "??s\u00a0"
    } else {
      todo <- bytes_total - bytes_done
      etas <- as.difftime(todo / rate, units = "secs")
      if (etas < 1) {
        parts$eta <- "<1s\u00a0\u00a0\u00a0"
      } else {
        parts$eta <- sp(format(pretty_dt(etas, compact = TRUE), width = 6))
      }
    }
  }

  parts
}

pkgplan__done_progress_bar <- function(bar) {
  if (!is.null(bar$status)) cli_status_clear(bar$status)
}
