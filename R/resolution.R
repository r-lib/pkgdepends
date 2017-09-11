
## API

remotes_resolve <- function(self, private) {
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL

  private$dirty <- TRUE
  private$resolution <- private$start_new_resolution()

  types <- c("cran", vapply(private$remotes, "[[", character(1), "type"))

  ## Get caches needed for resolutions
  lapply(unique(types), function(type) {
    private$resolution$cache[[type]] <- private$update_cache(type)
  })

  ## Resolve the remotes
  ## These will register themselves in private$resolution$packages
  lapply(private$remotes, private$resolve_ref)

  ## This is a synchronization barrier. After this, no async code is
  ## running, and we turn all deferred values into actual values.
  ## Order is important, because some async code might rely on the cache
  ## containing deferred values.
  private$resolution$packages <- await_env(private$resolution$packages)
  private$resolution$cache    <- await_env(private$resolution$cache)

  private$dirty <- FALSE
  self$get_resolution()
}

remotes_get_resolution <- function(self, private) {
  private$resolution_to_df()
}

remotes_diff_resolution <- function(self, private) {
  if (is.null(private$resolution)) stop("You need to resolve first")
  oldremotes <- remotes$new(private$repo)
  tryCatch(
    oldremotes$.__enclos_env__$private$load_resolution(),
    error = function(e) stop("Cannot load resolution")
  )

  current <- oldremotes$get_resolution()
  proposed <- self$get_resolution()

  structure(
    list(current = current, proposed = proposed),
    class = "remotes_resolution_diff"
  )
}

## Internals

remotes__resolve_ref <- function(self, private, rem) {
  ## We might have a deferred value for it already
  if (private$is_resolving(rem$ref)) {
    return(private$resolution$packages[[rem$ref]])
  }

  private$resolution$packages[[rem$ref]] <-
    if (rem$type == "cran") {
      private$resolve_ref_cran(rem)
    } else if (rem$type == "github") {
      private$resolve_ref_github(rem)
    }
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
