
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
  private$solution <- NULL

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
      private$resolution$metadata$resolution_end <- Sys.time()
      private$resolution$result <- remotes__resolution_to_df(
        private$resolution$packages,
        private$resolution$metadata,
        private$remotes,
        private$config$cache_dir)
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
    deps <- unique(unlist(lapply(get_files(res), function(x) x$deps$ref)))
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
  res$metadata <- list()
  res$metadata$resolution_start <- Sys.time()

  ## This is a generic cache
  res$cache <- new.env(parent = emptyenv())
  res$cache$progress_bar <- progress_bar
  res$cache$numdeps <- length(private$remotes)
  res$cache$numdeps_done <- 0

  res$cache$package_cache <-
    package_cache$new(private$config$package_cache_dir)

  res
}

remotes__resolution_to_df <- function(packages, metadata,
                                       remotes, cache_dir) {

  errs <- Filter(function(x) get_status(x) != "OK", packages)

  num_files <- viapply(packages, num_files)
  remote <- rep(lapply(packages, get_remote), num_files)
  ref <- rep(
    vcapply(packages, function(x) get_ref(x), USE.NAMES = FALSE),
    num_files
  )
  res_id <- rep(seq_along(packages), num_files)
  res_file <- unlist(lapply(packages, function(x) seq_len(num_files(x))))
  packages_subset <- lapply(seq_along(res_id), function(i) {
    r <- packages[[ res_id[i] ]]
    r <- set_files(r, get_files(r)[ res_file[i] ])
    r
  })

  getf <- function(f) {
    unlist(lapply(packages, function(x) vcapply(get_files(x), "[[", f)))
  }

  getfl <- function(f) {
    I(unlist(
      lapply(packages, function(x) lapply(get_files(x), "[[", f)),
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
    resolution = packages_subset
  )

  structure(
    list(data = res, metadata = metadata),
    class = "remotes_resolution"
  )
}

remotes__is_resolving <- function(self, private, ref) {
  ref %in% names(private$resolution$packages)
}

remotes__subset_resolution <- function(self, private, which) {
  "!DEBUG taking a subset of a resolution"
  current <- self$get_resolution()
  remotes__resolution_to_df(current$data$resolution[which],
                            current$metadata,
                            private$remotes,
                            private$config$cache_dir)
}

#' @importFrom prettyunits pretty_dt
#' @importFrom crayon bgBlue green blue white bold col_nchar
#' @importFrom cli cat_rule

print.remotes_resolution <- function(x, ...) {
  meta <- x$metadata
  x <- x$data

  direct <- unique(x$ref[x$direct])
  dt <- pretty_dt(meta$resolution_end - meta$resolution_start)
  head <- glue(
    "{logo()} RESOLUTION, {length(direct)} refs, resolved in {dt} ")
  width <- getOption("width") - col_nchar(head, type = "width") - 1
  head <- paste0(head, strrep(symbol$line, max(width, 0)))
  cat(blue(bold(head)), sep = "\n")

  print_refs(x, x$direct, header = NULL)

  print_refs(x, (! x$direct), header = "Dependencies", by_type = TRUE)

  print_failed_refs(x)

  invisible(x)
}

get_failed_refs <- function(res) {
  failed <- tapply(res$status, res$ref, function(x) all(x != "OK"))
  names(which(failed))
}

#' @importFrom crayon red

print_refs <- function(res, which, header, by_type = FALSE,
                       mark_failed = TRUE) {
  if (!length(res$ref[which])) return()

  if (!is.null(header)) cat(blue(bold(paste0(header, ":"))), sep = "\n")

  mark <- function(wh, short = FALSE) {
    ref <- ref2 <- sort(unique(res$ref[wh]))
    if (short) ref2 <- basename(ref)
    if (mark_failed) {
      failed_ref <- get_failed_refs(res[wh,])
      ref <- ifelse(ref %in% failed_ref, bold(red(ref2)), ref2)
    }
    ref2
  }

  if (by_type) {
    for (t in sort(unique(res$type[which]))) {
      cat(blue(paste0("  ", t, ":")), sep = "\n")
      which2 <- which & res$type == t
      cat(comma_wrap(mark(which2, short = t == "installed"), indent = 4),
          sep = "\n")
    }

  } else {
    cat(comma_wrap(mark(which)), sep = "\n")
  }
}

print_failed_refs <- function(res) {
  failed <- get_failed_refs(res)
  if (length(failed) > 0) cat(bold(red("Errors:")), sep = "\n")
  for (f in failed) print_failed_ref(res, f)
}

print_failed_ref <- function(res, failed_ref) {
  cat0("  ", failed_ref, ": ")
  wh <- which(failed_ref == res$ref)
  errs <- unique(vcapply(
    res$resolution[wh],
    function(x) {
      get_error_message(x) %||%
      get_files(x)[[1]]$error$message %||%
      "Unknown error"
    }
  ))
  cat(paste(errs, collapse = "\n    "), sep = "\n")
}
