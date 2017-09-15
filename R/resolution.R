
## API

remotes_resolve <- function(self, private) {
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL

  private$dirty <- TRUE
  private$resolution <- private$start_new_resolution()

  ## Resolve the remotes
  ## These will register themselves in private$resolution$packages
  lapply(private$remotes, private$resolve_ref)

  ## This is a synchronization barrier. After this, no async code is
  ## running, and we turn all deferred values into actual values.
  private$resolution$packages <- await_env(private$resolution$packages)

  private$dirty <- FALSE
  self$get_resolution()
}

remotes_get_resolution <- function(self, private) {
  private$resolution_to_df()
}

## Internals

remotes__resolve_ref <- function(self, private, rem) {
  ## We might have a deferred value for it already
  if (private$is_resolving(rem$ref)) {
    return(private$resolution$packages[[rem$ref]])
  }

  private$resolution$packages[[rem$ref]] <- resolve_remote(rem)
}

remotes__start_new_resolution <- function(self, private) {
  res <- new.env(parent = emptyenv())

  ## These are the resolved packages. They might be deferred values,
  ## if the resolution is still ongoing.
  res$packages <- new.env(parent = emptyenv())

  ## This is a generic cache
  res$cache <- new.env(parent = emptyenv())

  res
}

remotes__resolution_to_df <- function(self, private) {
  remotes <- private$remotes
  resolution <- private$resolution

  ress <- resolution$packages
  errs <- Filter(function(x) x$status != "OK", ress)

  num_files <- viapply(ress, function(x) length(x$files))
  ref <- rep(
    vcapply(
      ress,
      function(x) x$remote$origref %||% x$remote$ref,
      USE.NAMES = FALSE
    ),
    num_files
  )

  getf <- function(f) {
    unlist(lapply(ress, function(x) vcapply(x$files, "[[", f)))
  }

  sources <- I(unlist(
    lapply(ress, function(x) lapply(x$files, "[[", "source")),
    recursive = FALSE
  ))

  res <- structure(data.frame(
    stringsAsFactors = FALSE,
    ref      = ref,
    direct   = ref %in% vcapply(remotes, "[[", "ref"),
    status   = getf("status"),
    package  = getf("package"),
    version  = getf("version"),
    platform = getf("platform"),
    rversion = getf("rversion"),
    repodir  = getf("dir"),
    sources  = sources,
    target   = getf("target")
  ), class = c("remotes_resolution", "data.frame"))

  rownames(res) <- NULL
  res
}

remotes__is_resolving <- function(self, private, ref) {
  ref %in% names(private$resolution$packages)
}
