
## API

#' @importFrom progress progress_bar

remotes_resolve <- function(self, private) {
  progress_bar <- progress_bar$new(
    total = length(private$remotes),
    format = "  Resolving dependencies [:current/:total] [:deps/:deptot] :elapsedfull"
  )
  progress_bar$tick(0, tokens = list(deps = 0, deptot = 0))

  await(self$async_resolve(progress_bar = progress_bar))

  progress_msg("Resolving dependencies")
}

remotes_async_resolve <- function(self, private, progress_bar = NULL) {
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL

  private$dirty <- TRUE
  private$resolution <- private$start_new_resolution(progress_bar)

  pool <- deferred_pool$new()
  proms <- lapply(private$remotes, private$resolve_ref, pool = pool)

  if (!is.null(private$resolution$cache$progress_bar)) {
    for (pr in proms) {
      pr$then(function() {
        private$resolution$cache$progress_bar$tick(
          tokens = list(
            deps = private$resolution$cache$numdeps_done,
            deptot = private$resolution$cache$numdeps)
        )
      })
    }
  }

  res <- pool$when_complete()$
    then(function() {
      private$resolution$packages <-
        eapply(private$resolution$packages, get_async_value)
    })$
    then(function() private$dirty <- FALSE)$
    then(function() self$get_resolution())

  pool$finish()

  res
}

remotes_get_resolution <- function(self, private) {
  private$resolution_to_df()
}

## Internals

remotes__resolve_ref <- function(self, private, rem, pool) {
  ## We might have a deferred value for it already
  if (private$is_resolving(rem$ref)) {
    return(private$resolution$packages[[rem$ref]])
  }

  cache <- private$resolution$cache

  dres <- resolve_remote(rem, config = private$config, cache = cache)

  if (!is_deferred(dres)) dres <- async_constant(dres)

  private$resolution$packages[[rem$ref]] <- dres
  pool$add(dres)

  deps <- dres$then(function(res) {
    deps <- unique(unlist(lapply(res$files, "[[", "deps")))
    cache$numdeps <- cache$numdeps + length(deps)
    lapply(parse_remotes(deps), private$resolve_ref, pool = pool)
  })
  pool$add(deps)

  dres
}

remotes__start_new_resolution <- function(self, private, progress_bar) {
  res <- new.env(parent = emptyenv())

  ## These are the resolved packages. They might be deferred values,
  ## if the resolution is still ongoing.
  res$packages <- new.env(parent = emptyenv())

  ## This is a generic cache
  res$cache <- new.env(parent = emptyenv())
  res$cache$progress_bar <- progress_bar
  res$cache$numdeps <- length(private$remotes)
  res$cache$numdeps_done <- 0

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
    ref        = ref,
    direct     = ref %in% vcapply(remotes, "[[", "ref"),
    status     = getf("status"),
    package    = getf("package"),
    version    = getf("version"),
    platform   = getf("platform"),
    rversion   = getf("rversion"),
    repodir    = getf("dir"),
    sources    = sources,
    target     = getf("target"),
    fulltarget = file.path(private$config$cache_dir, getf("target"))
  ), class = c("remotes_resolution", "data.frame"))

  rownames(res) <- NULL
  res
}

remotes__is_resolving <- function(self, private, ref) {
  ref %in% names(private$resolution$packages)
}
