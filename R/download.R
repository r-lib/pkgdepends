
remotes_download_resolution <- function(self, private) {
  remotes_download_internal(self, private, which = TRUE)
}

remotes_async_download_resolution <- function(self, private) {
  remotes_async_download_internal(self, private, which = TRUE)
}

remotes_download_solution <- function(self, private) {
  TODO
}

remotes_async_download_solution <- function(self, private) {
  TODO
}

remotes_download_internal <- function(self, private, which) {
  progress_bar <- progress_bar$new(
    total = private$get_total_files(),
    format = "  Downloading files     [:current/:total] :elapsedfull"
  )
  progress_bar$tick(0)

  await(remotes_async_download_internal(self, private, which,
                                        progress_bar = progress_bar))

  progress_msg("Downloading files")
}

remotes_async_download_internal <- function(self, private, which,
                                            progress_bar) {
  if (is.null(private$resolution)) self$resolve()

  if (private$dirty) stop("Need to resolve, remote list has changed")

  resolution <- self$get_resolution()
  if (any(resolution$status != "OK")) {
    stop("Resolution has errors, cannot start downloading")
  }

  private$resolution$cache$progress_bar <- progress_bar

  dls <- async_map(
    private$resolution$packages[which],
    private$download_res
  )

  dls$then(function(value) {
    private$downloads <- value
    self$get_resolution_download()
  })
}



remotes_download_res <- function(self, private, res) {

  force(private)

  ddl <- download_remote(
    res,
    config = private$config,
    cache = private$resolution$cache
  )

  if (!is_deferred(ddl)) ddl <- async_constant(ddl)

  if (!is.null(private$resolution$cache$progress_bar)) {
    ddl$then(function() {
      cache <- private$resolution$cache
      cache$progress_bar$tick(length(res$files))
    })
  }

  ddl
}

## This has the same structure as the resolutions, but we add some
## extra columns

remotes_get_resolution_download <- function(self, private) {
  if (is.null(private$downloads)) stop("No downloads")
  reso <- self$get_resolution()
  dl <- as.list(private$downloads)

  getf <- function(f) unlist(lapply(dl, function(x) lapply(x, "[[", f)))

  errors <- unlist(
    lapply(dl, function(x) lapply(x, function(xx) as.character(xx$error))),
    recursive = FALSE
  )

  reso$download_status <- getf("status")
  reso$bytes <- getf("bytes")
  reso$errors <- I(errors)

  class(reso) <- c("remotes_downloads", "data.frame")
  reso
}
