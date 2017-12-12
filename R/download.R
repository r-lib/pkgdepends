
remotes_download_resolution <- function(self, private) {
  synchronise(self$async_download_resolution())
}

remotes_async_download_resolution <- function(self, private) {
  if (is.null(private$resolution)) self$resolve()
  if (private$dirty) stop("Need to resolve, remote list has changed")
  dls <- remotes_async_download_internal(
    self, private, private$resolution$result$data$resolution
  )

  dls$then(function(value) {
    private$downloads <- value
    self$get_resolution_download()
  })
}

remotes_download_solution <- function(self, private) {
  synchronise(self$async_download_solution())
}

remotes_async_download_solution <- function(self, private) {
  if (is.null(private$solution)) self$solve()
  if (private$dirty) stop("Need to resolve, remote list has changed")

  dls <- remotes_async_download_internal(
    self, private, private$solution$result$data$resolution)

  dls$then(function(value) {
    private$solution_downloads <- value
    self$get_solution_download()
  })
}

remotes_async_download_internal <- function(self, private, what) {
  if (any(vcapply(what, get_status) != "OK")) {
    stop("Resolution has errors, cannot start downloading")
  }
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

  reso$data$download_status <- getf("status")
  reso$data$bytes <- getf("bytes")
  reso$data$errors <- I(errors)

  class(reso) <- c("remotes_downloads", class(reso))
  reso
}
