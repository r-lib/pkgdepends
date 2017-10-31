
setup_download_progress_bar <- function(total) {
  progress_bar <- progress_bar$new(
    total = total,
    format = "  Downloading files     [:current/:total] :elapsedfull"
  )
  progress_bar$tick(0)
  progress_bar
}

remotes_download_resolution <- function(self, private) {
  total <- nrow(private$resolution$result)
  progress_bar <- setup_download_progress_bar(total)
  synchronise(self$async_download_resolution(progress_bar = progress_bar))
}

remotes_async_download_resolution <- function(self, private, progress_bar) {
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")
  dls <- remotes_async_download_internal(
    self, private, private$resolution$packages, progress_bar)

  dls$then(function(value) {
    private$downloads <- value
    self$get_resolution_download()
  })
}

remotes_download_solution <- function(self, private) {
  total <- nrow(private$solution$result)
  progress_bar <- setup_download_progress_bar(total)
  synchronise(self$async_download_solution(progress_bar = progress_bar))
}

remotes_async_download_solution <- function(self, private, progress_bar) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  dls <- remotes_async_download_internal(
    self, private, private$solution$packages, progress_bar)

  dls$then(function(value) {
    private$solution_downloads <- value
    self$get_solution_download()
  })
}

remotes_async_download_internal <- function(self, private, what,
                                            progress_bar) {
  if (any(what$status != "OK")) {
    stop("Resolution has errors, cannot start downloading")
  }

  private$resolution$cache$progress_bar <- progress_bar

  async_map(what, private$download_res)
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
  remotes_get_download(private$resolution$result, private$downloads)
}

remotes_get_solution_download <- function(self, private) {
  if (is.null(private$solution_downloads)) stop("No downloads")
  remotes_get_download(private$solution$result,
                       private$solution_downloads)
}

remotes_get_download <- function(resolution, downloads) {
  reso <- resolution
  dl <- downloads

  getf <- function(f) unlist(lapply(dl, function(x) lapply(x, "[[", f)))

  errors <- unlist(
    lapply(dl, function(x) lapply(x, function(xx) as.character(xx$error))),
    recursive = FALSE
  )

  reso$download_status <- getf("status")
  reso$bytes <- getf("bytes")
  reso$errors <- I(errors)

  class(reso) <- c("remotes_downloads", class(reso))
  reso
}
