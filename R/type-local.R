
### -----------------------------------------------------------------------
### API

#' @export

parse_remote.remote_specs_local <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_local_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "local"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

#' @export

resolve_remote.remote_ref_local <- function(remote, config, ...,
                                            cache) {

  tryCatch({
    dsc <- desc(file = remote$path)

    deps <- resolve_ref_deps(
      dsc$get_deps(), dsc$get("Remotes")[[1]], config$dependencies)

    rversion <- tryCatch(
      get_minor_r_version(dsc$get_built()$R),
      error = function(e) "*"
    )

    platform <- tryCatch(
      dsc$get_built()$Platform %|z|% "source",
      error = function(e) "source"
    )

    files <- list(
      source = remote$path,
      target = file.path("src", "contrib", basename(remote$path)),
      platform = platform,
      rversion = rversion,
      dir = file.path("src", "contrib"),
      package = dsc$get("Package")[[1]],
      version = dsc$get("Version")[[1]],
      deps = deps,
      status = "OK"
    )
    remote$description <- dsc

    structure(
      list(files = list(files), remote = remote, status = "OK"),
      class = c("remote_resolution_local", "remote_resolution")
    )

  }, error = function(err) {
    files <- list(
      source = remote$path,
      target = file.path("src", "contrib", basename(remote$path)),
      platform = NA_character_,
      rversion = NA_character_,
      dir = file.path("src", "contrib"),
      package = NA_character_,
      version = NA_character_,
      deps = NULL,
      status = "FAILED",
      error = err
    )
    remote["description"] <- list(NULL)

    structure(
      list(files = list(files), remote = remote, status = "FAILED"),
      class = c("remote_resolution_local", "remote_resolution")
    )
  })
}

#' @export

download_remote.remote_resolution_local <- function(resolution, config,
                                                    ..., cache) {
  tryCatch({
    files <- resolution$files[[1]]
    target_file <- file.path(config$cache_dir, files$target)
    mkdirp(dirname(target_file))
    file.copy(files$source, target_file)
    async_constant(list(make_dl_status("Had", files$source, target_file,
                                       bytes = file.size(target_file))))
  }, error = function(err) {
    async_constant(list(make_dl_status("Failed", files$source,
                                       target_file, error = err)))
  })
}

#' @export

satisfies_remote.remote_resolution_local <-
  function(resolution, candidate, config, ...) {
    ## TODO: we can probably do better than this
    FALSE
  }

## ----------------------------------------------------------------------
## Internal functions

type_local_rx <- function() {
  paste0(
    "^",
    "(?:local::)",
    "(?<path>.*)",
    "$"
  )
}
