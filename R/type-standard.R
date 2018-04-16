
## ------------------------------------------------------------------------
## API

parse_remote_standard <- function(specs, config, ...) {

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

resolve_remote_standard <- function(remote, direct, config,
                                    cache, dependencies, ...) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

download_remote_standard <- function(resolution, config,
                                     mode, ..., cache,
                                     progress_bar) {
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

satisfy_remote_standard <- function(resolution, candidate, config, ...) {

  ## A standard ref is special, in that any ref source can satisfy it,
  ## as long as the package name is the same, and the version
  ## requirements are satisfied.

  ## 1. package name must be the same
  if (resolution$package != candidate$package) return(FALSE)

  ## 2. version requirements must be satisfied
  if (resolution$remote[[1]]$version == "") return(TRUE)

  version_satisfies(
    candidate$version,
    resolution$remote[[1]]$atleast,
    resolution$remote[[1]]$version
  )
}
