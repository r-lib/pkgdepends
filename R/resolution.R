
## API

#' @importFrom progress progress_bar

remotes_resolve <- function(self, private) {
  "!DEBUG remotes_resolve (sync)"
  progress_bar <- progress_bar$new(
    total = length(private$remotes),
    format = "  Resolving dependencies [:current/:total] [:deps/:deptot] :elapsedfull"
  )
  progress_bar$tick(0, tokens = list(deps = 0, deptot = 0))

  res <- synchronise(self$async_resolve(progress_bar = progress_bar))

  progress_msg("Resolving dependencies")

  res
}

remotes_async_resolve <- function(self, private, progress_bar = NULL) {
  "!DEBUG remotes_resolve (async)"
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
    then(function() {
      private$resolution$result <-
        private$resolution_to_df(private$resolution$packages)
    })

  pool$finish()

  res
}

remotes_get_resolution <- function(self, private) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  private$resolution$result
}

## Internals

remotes__resolve_ref <- function(self, private, rem, pool) {
  "!DEBUG resolving `rem$ref` (type `rem$type`)"
  ## We might have a deferred value for it already
  if (private$is_resolving(rem$ref)) {
    return(private$resolution$packages[[rem$ref]])
  }

  cache <- private$resolution$cache

  dres <- resolve_remote(rem, config = private$config, cache = cache)
  if (!is_deferred(dres)) dres <- async_constant(dres)
  private$resolution$packages[[rem$ref]] <- dres
  pool$add(dres)

  if (!is.null(private$library) &&
      !is.null(rem$package) &&
      file.exists(file.path(private$library, rem$package))) {
    lib <- normalizePath(private$library, winslash = "/",
                         mustWork = FALSE)
    ref <- paste0("installed::", lib, "/", rem$package)
    if (! ref %in% names(private$resolution$packages)) {
      private$resolve_ref(parse_remotes(ref)[[1]], pool = pool)
    }
  }

  deps <- dres$then(function(res) {
    deps <- unique(unlist(lapply(res$files, function(x) x$deps$ref)))
    cache$numdeps <- cache$numdeps + length(deps)
    lapply(parse_remotes(deps), private$resolve_ref, pool = pool)
  })
  pool$add(deps)

  dres
}

remotes__start_new_resolution <- function(self, private, progress_bar) {
  "!DEBUG cleaning up for new resolution"
  res <- new.env(parent = emptyenv())

  ## These are the resolved packages. They might be deferred values,
  ## if the resolution is still ongoing.
  res$packages <- new.env(parent = emptyenv())

  ## This is a generic cache
  res$cache <- new.env(parent = emptyenv())
  res$cache$progress_bar <- progress_bar
  res$cache$numdeps <- length(private$remotes)
  res$cache$numdeps_done <- 0

  res$cache$package_cache <-
    package_cache$new(private$config$package_cache_dir)

  res
}

remotes__resolution_to_df <- function(self, private, resolution) {
  "!DEBUG formatting resolution into data frame"
  remotes_i_resolution_to_df(resolution, private$remotes,
                             private$config$cache_dir)
}

remotes_i_resolution_to_df <- function(resolution, remotes, cache_dir) {
  errs <- Filter(function(x) x$status != "OK", resolution)

  num_files <- viapply(resolution, function(x) length(x$files))
  remote <- rep(
    lapply(resolution, function(x) x$remote),
    num_files
  )
  ref <- rep(
    vcapply(resolution, function(x) x$remote$ref, USE.NAMES = FALSE),
    num_files
  )
  res_id <- rep(seq_along(resolution), num_files)
  res_file <- unlist(lapply(resolution, function(x) seq_along(x$files)))
  resolution_subset <- lapply(seq_along(res_id), function(i) {
    r <- resolution[[ res_id[i] ]]
    r$files <- r$files[ res_file[i] ]
    r
  })

  getf <- function(f) {
    unlist(lapply(resolution, function(x) vcapply(x$files, "[[", f)))
  }

  getfl <- function(f) {
    I(unlist(
      lapply(resolution, function(x) lapply(x$files, "[[", f)),
      recursive = FALSE
    ))
  }

  sources <- getfl("source")
  deps <- getfl("deps")

  res <- tibble::tibble(
    ref        = ref,
    type       = vcapply(remote, "[[", "type"),
    direct     = ref %in% vcapply(remotes, "[[", "ref"),
    status     = getf("status"),
    package    = getf("package"),
    version    = getf("version"),
    platform   = getf("platform"),
    rversion   = getf("rversion"),
    repodir    = getf("dir"),
    sources    = sources,
    target     = getf("target"),
    fulltarget = file.path(cache_dir, getf("target")),
    dependencies = deps,
    remote     = remote,
    resolution = resolution_subset
  )
  class(res) <- c("remotes_resolution", class(res))

  res
}

remotes__is_resolving <- function(self, private, ref) {
  ref %in% names(private$resolution$packages)
}

remotes__subset_resolution <- function(self, private, which) {
  "!DEBUG taking a subset of a resolution"
  df <- self$get_resolution()
  df$resolution[which]
}

print.remotes_resolution <- function(x, ...) {
  print_refs(x, x$direct, header = "Remote resolution for refs")
  print_refs(
    x, (! x$direct) & (x$type != "installed"),
    header = "Dependencies",
    by_type = TRUE)
  print_refs(
    x, (! x$direct) & (x$type == "installed"),
    header = "Already installed")
  ## TODO: time stamp
  ## TODO: Errors
  invisible(x)
}

print_refs <- function(res, which, header, by_type = FALSE) {
  if (!length(res$ref[which])) return()

  cat(paste0(header, ":"), sep = "\n")

  if (by_type) {
    for (t in sort(unique(res$type[which]))) {
      cat(paste0("  ", t, ":"), sep = "\n")
      which2 <- which & res$type == t
      l <- comma_wrap(sort(unique(res$ref[which2])), indent = 4)
      cat(l, sep = "\n")
    }

  } else {
    l <- comma_wrap(sort(unique(res$ref[which])))
    cat(l, sep = "\n")
  }
}
