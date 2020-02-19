
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
#' 230 kB/s (#####      ) 45% | 14/56 pkgs | ETA ~34s | Getting dplyr
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
#' 230 kB/s (#####      ) 45% | 14/56 pkgs | Getting dplyr
#' ```
#'
#' * Percent is for the packages, not bytes.
#' * We can't really show ETA.
#'
#' ## Events
#'
#' This is quite simple currently, we just print the packages we are
#' "getting" currently, i.e. the packages we received data for in the
#' last time slot.
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

  bar$what <- what[, c("type", "filesize", "package", "cache_status")]
  bar$what$skip <-
    what$type %in% c("installed", "deps") |
    what$cache_status != "miss"

  unk <- sum(is.na(bar$what$filesize[!bar$what$skip]))
  num <- sum(!bar$what$skip) - unk
  bts <- sum(bar$what$filesize[!bar$what$skip], na.rm = TRUE)
  cli_alert_info(c(
    "{.alert-info About to download",
    if (num > 0) " {num} package{?s}",
    if (bts > 0) " ({pretty_bytes(bts)})",
    if (num > 0 && unk > 0) " and",
    if (unk > 0) " {unk} package{?s} with unknown size",
    "}"
  ))
  bar$status <- cli_status("", .auto_close = FALSE)

  bar$what$idx <- seq_len(nrow(what))
  bar$what$current <- 0L         # We got this many bytes
  bar$what$status <- "todo"      # "todo", "data", "got", "had", "error"

  bar$chars <- progress_chars()
  bar$chunks <- new.env(parent = emptyenv())
  bar$start_at <- Sys.time()

  bar$timer <- new_async_timer(
    1/10,
    function() pkgplan__show_progress_bar(bar)
  )
  bar$timer$listen_on("error", function(e) { stop(e) })

  bar$events <- list()                  # got, had, error, data
  bar$lastmsg <- "Connecting..."

  bar
}

#' Update the progress bar data
#'
#' This is triggered by libcurl.
#'
#' @param bar The progress bar.
#' @param idx The row index in the download data table.
#' @param data Data about this row, a list with entries `current` and
#'   `total`, measured in the number of bytes. If `total` is unknown,
#'   then it is set to zero. At the end of the download we also get
#'   `"done"`, and on error `"error"`.
#'
#' @keywords internal
#' @importFrom cli cli_alert_success cli_alert_danger

pkgplan__update_progress_bar <- function(bar, idx, event, data) {
  # Record the time here, and use it in this function, so that this
  # function runs in a single point of time
  time <- Sys.time()

  # Work out which second we are in. We record the received data per second
  sec <- as.character(floor(as.double(time - bar$start_at, units = "secs")))

  # If it is "done", then we are done. We don't make assumptions about
  # libcurl/async signalling the end of every file via file sizes in the
  # progress bar callback, so we assume that when we get "done", then we
  # are indeed "done" and set the status and sizes accordingly.
  if (event == "done") {
    if (data$download_status == "Got") {
      bar$what$status[idx] <- "got"
      sz <- na.omit(file.size(c(data$fulltarget, data$fulltarget_tree)))[1]
      cli_alert_success(c(
        "Got {.pkg {data$package}} ",
        "{.version {data$version}} ({data$platform}) ",
        if (!is.na(sz)) "{.size ({pretty_bytes(sz)})}"
      ))
      if (!is.na(bar$what$filesize[idx])) {
        bar$chunks[[sec]] <- (bar$chunks[[sec]] %||% 0) -
          bar$what$current[idx] + bar$what$filesize[idx]
        bar$what$current[idx] <- bar$what$filesize[idx]
      }
    } else {
      bar$what$status[idx] <- "had"
      bar$what$current[idx] <- 0L
      if (data$cache_status == "miss") cli_alert_success(c(
        "Cached copy of {.pkg {data$package}} ",
        "{.version {data$version}} ({data$platform}) is the latest build"
      ))
    }

    return(TRUE)
  }

  if (event == "error") {
    cli_alert_danger(c(
      "Failed to download {.pkg {data$package}} ",
      "{.version {data$version}} ({data$platform})"
    ))
    bar$what$status[idx] <- "error"
  }

  # Otherwise we got a chunk of data
  bar$what$status[idx] <- "data"
  bar$events$data <- unique(c(bar$events$data, idx))

  # Update data chunks
  bar$chunks[[sec]] <- (bar$chunks[[sec]] %||% 0) -
    bar$what$current[idx] + data$current

  # Update current and total
  bar$what$current[idx] <- data$current
  if (data$total > 0) bar$what$filesize[idx] <- data$total

  TRUE
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

  bar$events <- list()
  cli_status_update(bar$status, str)
}

calculate_progress_parts <- function(bar) {

  parts <- list()

  sp <- function(x) gsub(" ", "\u00a0", x)

  # We filter these here, instead at the beginning, because otherwise
  # the indices would not work in the update function
  whatx <- bar$what[! bar$what$skip, ]

  # Simple numbers
  parts$pkg_done <- sum(whatx$status %in% c("got", "had", "error"))
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
  parts$msg <- bar$lastmsg
  if (length(bar$events$data) > 0) {
    pkgs <- bar$what$package[bar$what$idx %in% bar$events$data]
    parts$msg <- paste0(
      "Getting ",
      glue_collapse(pkgs, sep = ", ", last = " and ")
    )
    bar$lastmsg <- parts$msg
  }

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
  # TODO: print a summary?
  if (!is.null(bar$status)) cli_status_clear(bar$status)
}
