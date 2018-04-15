
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
  force(remote); force(direct); force(dependencies)

  cache$metadata$async_deps(remote$package, dependencies = dependencies)$
    then(function(x) {
      res <- x[c("ref", "type", "status", "package", "version", "license",
                 "needscompilation", "priority", "md5sum", "built",
                 "platform", "rversion", "repodir", "target", "deps",
                 "sources")]
      res$ref[res$package == remote$package] <- remote$ref
      res$needscompilation <-
        tolower(res$needscompilation) %in% c("yes", "true")
      res$direct <- direct & res$ref == remote$ref
      res$type <- "standard"
      res
    })
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
