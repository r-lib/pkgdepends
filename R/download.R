
#' @importFrom prettyunits pretty_bytes

remotes_download_resolution <- function(self, private) {
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")
  synchronise(self$async_download_resolution())
}

remotes_async_download_resolution <- function(self, private) {
  self ; private
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  remotes_async_download_internal(self, private,
                                  private$resolution$result)$
    then(function(value) {
      private$downloads <- value
      self$get_resolution_download()
    })
}

remotes_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) stop("Need to resolve, remote list has changed")
  synchronise(self$async_download_solution())
}

remotes_async_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  remotes_async_download_internal(self, private,
                                  private$solution$result$data)$
    then(function(value) {
      private$solution_downloads <- value
      self$get_solution_download()
    })
}

remotes_stop_for_solution_download_error <- function(self, private) {
  dl <- self$get_solution_download()
  if (any(bad <- tolower(dl$download_status) == "failed")) {
    msgs <- vcapply(
      which(bad),
      function(i) {
        urls <- format_items(dl$sources[[i]])
        glue("Failed to download {dl$package[i]} \\
              from {urls}.")
      }
    )
    msg <- paste(msgs, collapse = "\n")
    stop("Cannot download some packages:\n", msg, call. = FALSE)
  }
}

remotes_async_download_internal <- function(self, private, what) {
  if (any(what$status != "OK")) {
    stop("Resolution has errors, cannot start downloading")
  }
  start <- Sys.time()
  private$progress_bar <- private$create_progress_bar(what)

  dl <- lapply(seq_len(nrow(what)), function(idx) {
    force(idx)
    private$download_res(
      what[idx, ],
      on_progress = function(data) {
        private$update_progress_bar(idx, data)
      })$
      finally(function() private$update_progress_bar(idx, "done"))
  })

  when_all(.list = dl)$
    then(function(dls) {
      what$fulltarget <- vcapply(dls, "[[", "fulltarget")
      what$download_status <- vcapply(dls, "[[", "download_status")
      what$download_error <- lapply(dls, function(x) x$download_error[[1]])
      what$file_size <- vdapply(dls, "[[", "file_size")
      class(what) <- c("remotes_downloads", class(what))
      attr(what, "metadata")$download_start <- start
      attr(what, "metadata")$download_end <- Sys.time()
      what
    })$
    finally(function() private$done_progress_bar())
}

remotes_download_res <- function(self, private, res, on_progress) {
  force(private)
  download_remote(
    res,
    config = private$config,
    cache = private$cache,
    on_progress = on_progress
  )
}

download_remote <- function(res, config, cache, on_progress = NULL,
                            remote_types = NULL) {
  remote_types <- c(default_remote_types(), remote_types)
  dl <- remote_types[[res$type]]$download %||% type_default_download
  target <- file.path(config$cache_dir, res$target)
  mkdirp(dirname(target))
  async(dl)(res, target, config, cache = cache, on_progress = on_progress)$
    then(function(s) {
      if (length(res$sources[[1]]) && !file.exists(target)) {
        stop("Failed to download ", res$type, " package ", res$package)
      }
      if (!identical(s, "Had") && !identical(s, "Got") &&
          !identical(s, "Current")) s <- "Got"
      dlres <- res
      dlres$fulltarget <- target
      dlres$download_status <- s
      dlres$download_error <- list(NULL)
      dlres$file_size <- file.size(target)
      dlres
    })$
    catch(error = function(err) {
      dlres <- res
      dlres$fulltarget <- target
      dlres$download_status <- "Failed"
      dlres$download_error <- list(err)
      dlres$file_size <- NA_integer_
      dlres
    })
}

download_ping_if_not_source <- function(resolution, target, config, cache,
                                        on_progress) {
  resolution; target; config; cache; on_progress
  mkdirp(dirname(target))

  if (resolution$platform == "source") {
    ## If it is a source package, then the package name, version number
    ## and package type must match. If there is such a package in the cache
    ## we just take it
    cache$package$async_copy_or_add(
      target, resolution$sources[[1]], path = resolution$target,
      package = resolution$package, version = resolution$version,
      platform = resolution$platform)$
    then(~ attr(., "action"))

  } else {
    ## If not a source package, then we try to update it, in case there is
    ## a newly built binary
    cache$package$async_update_or_add(
      target, resolution$sources[[1]], path = resolution$target,
      package = resolution$package, version = resolution$version,
      platform = resolution$platform)$
    then(~ attr(., "action"))
  }
}

remotes_get_resolution_download <- function(self, private) {
  if (is.null(private$downloads)) stop("No downloads")
  private$downloads
}

remotes_get_solution_download <- function(self, private) {
  if (is.null(private$solution_downloads)) stop("No downloads")
  private$solution_downloads
}

#' @export

print.remotes_downloads <- function(x, ...) {
  cat(format.remotes_downloads(x, ...))
}

#' @export

format.remotes_downloads <- function(x, ...) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  meta <- attr(x, "metadata")

  direct <- unique(x$ref[x$direct])
  dt <- pretty_dt(meta$download_end - meta$download_start)
  head <- glue(
    "PKG DOWNLOADS, {length(direct)} refs, downloaded in {dt} ")
  width <- getOption("width") - col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(symbol$line, max(width, 0)))
  push(blue(bold(head)), sep = "\n")

  push(format_dls(x, x$direct, header = NULL))
  push(format_dls(x, (! x$direct), header = "Dependencies", by_type = TRUE))
  push(format_failed_dls(x))

  paste0(result, collapse = "")
}

get_failed_dls <- function(dls) {
  dls$ref[dls$download_status == "Failed"]
}

format_dls <- function(dls, which, header, by_type = FALSE,
                       mark_failed = TRUE) {
  if (!length(dls$ref[which])) return()

  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  if (!is.null(header)) push(blue(bold(paste0(header, ":"))), sep = "\n")

  mark <- function(wh, short = FALSE) {
    ref <- ref2 <- sort(unique(dls$ref[wh]))
    if (short) ref2 <- basename(ref)
    if (mark_failed) {
      failed_dls <- get_failed_dls(dls[wh,])
      ref2 <- ifelse(ref %in% failed_dls, bold(red(ref2)), ref2)
    }
    ref2
  }

  if (by_type) {
    for (t in sort(unique(dls$type[which]))) {
      push(blue(paste0("  ", t, ":")), sep = "\n")
      which2 <- which & dls$type == t
      push(comma_wrap(mark(which2, short = t == "installed"), indent = 4),
           sep = "\n")
    }

  } else {
    push(comma_wrap(mark(which)), sep = "\n")
  }

  paste0(result, collapse = "")
}

format_failed_dls <- function(res) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  failed <- get_failed_dls(res)
  if (length(failed) > 0) push(bold(red("Errors:")), sep = "\n")
  for (f in failed) push(format_failed_dl(res, f))

  paste0(result, collapse = "")
}

format_failed_dl <- function(dls, failed_dl) {
  result <- character()
  push <- function(..., sep = "") result <<- c(result, paste0(c(...), sep))

  push("  ", failed_dl, ": ")
  wh <- which(failed_dl == dls$ref & dls$download_status == "Failed")
  errs <- unique(vcapply(dls$download_error[wh], conditionMessage))
  push(paste(errs, collapse = "\n    "), sep = "\n")

  paste0(result, collapse = "")
}

type_default_download <- function(resolution, target, config, cache,
                                  on_progress) {
  ## TODO
  stop("Not implemented yet")
}
