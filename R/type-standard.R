
## ------------------------------------------------------------------------
## API

#' @export

parse_remote.remote_specs_standard <- function(specs, config, ...) {

  ## This is the same as CRAN, but possibly with standard::
  parsed_specs <- re_match(specs, standard_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "standard"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

#' @export

resolve_remote.remote_ref_standard <- function(remote, direct, config,
                                               cache, dependencies, ...) {
  force(remote); force(direct); force(dependencies)

  cache$crandata <- cache$crandata %||% update_crandata_cache(config)
  cache$biocdata <- cache$biocdata %||% update_biocdata_cache(config)

  ## We try both cran and bioc
  cran <- cache$crandata$
    then(function(cacheresult) {
      type_cran_resolve_from_cache(remote, direct, config, cacheresult,
                                   dependencies)
    })

  bioc <- cache$biocdata$
    then(function(cacheresult) {
      type_bioc_resolve_from_cache(remote, direct, config, cacheresult,
                                   dependencies)
    })

  when_all(cran = cran, bioc = bioc)$
    then(function(val) {
      cran_status <- vcapply(get_files(val$cran), "[[", "status")
      bioc_status <- vcapply(get_files(val$bioc), "[[", "status")
      statusok <- any(cran_status == "OK") || any(bioc_status == "OK")
      files <- if (statusok) {
        c(get_files(val$cran)[cran_status == "OK"],
          get_files(val$bioc)[bioc_status == "OK"])
      } else {
        c(get_files(val$cran), get_files(val$bioc))
      }
      structure(
        list(
          files = files,
          direct = direct,
          remote = remote,
          status = if (statusok) "OK" else "FAILED"
        ),
        class = c("remote_resolution_standard", "remote_resolution")
      )
    })
}

#' @export

download_remote.remote_resolution_standard <- function(resolution, config,
                                                       ..., cache, progress_bar) {
  meta0 <- list(
    type = get_remote(resolution)[["type"]],
    ref = get_ref(resolution))

  async_map(get_files(resolution), function(file) {
    meta <- c(meta0, file[c("platform", "package", "version", "rversion")])
    get_package_from(cache$package_cache, file$source,
                     config$cache_dir, file$target, metadata = meta,
                     get_direct(resolution), progress_bar = progress_bar)
  })
}

#' @export

satisfies_remote.remote_resolution_standard <-
  function(resolution, candidate, config, ...) {

    ## A standard ref is special, in that any ref source can satisfy it,
    ## as long as the package name is the same, and the version
    ## requirements are satisfied.

    ## 1. package name must be the same
    if (get_remote(resolution)$package != get_remote(candidate)$package) {
      return(FALSE)
    }

    ## 2. version requirements must be satisfied
    if (get_remote(resolution)$version == "") return(TRUE)

    version_satisfies(
      get_files(candidate)[[1]]$version,
      get_remote(resolution)$atleast,
      get_remote(resolution)$version
    )
  }
