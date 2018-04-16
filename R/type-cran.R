
## ------------------------------------------------------------------------
## API

#' @importFrom rematch2 re_match
#' @importFrom stats na.omit
#' @importFrom desc desc_get_deps
#' @importFrom tibble as_tibble tibble

parse_remote_cran <- function(specs, ...) {

  parsed_specs <- re_match(specs, standard_rx("cran"))

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "cran"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_cran <- function(remote, direct, config, cache,
                                dependencies, ...) {
  force(remote); force(direct); force(dependencies)
  if (remote$version %in% c("", "current")) {
    type_cran_resolve_current(remote, direct, config, cache, dependencies)
  } else {
    type_cran_resolve_version(remote, direct, config, cache, dependencies)
  }
}

download_remote_cran <- function(resolution, config, mode, ..., cache,
                                 progress_bar) {

  meta0 <- list(
    type = get_remote(resolution)[["type"]],
    ref = get_ref(resolution))

  async_map(get_files(resolution), function(files) {
    meta <- c(meta0, files[c("platform", "package", "version", "rversion")])
    get_package_from(cache$package_cache, files$source,
                     config$cache_dir, files$target, metadata = meta,
                     get_direct(resolution), progress_bar = progress_bar)
  })
}

satisfy_remote_cran <- function(resolution, candidate, config, ...) {

  ## 1. candidate must be a cran, standard or installed ref
  if (!candidate$type %in% c("cran", "standard", "installed")) return(FALSE)

  ## 2. installed refs must be from CRAN
  if (candidate$type == "installed") {
    dsc <- candidate$remote[[1]]$description
    if (!is.null(dsc) &&
        ! identical(dsc$get("Repository")[[1]], "CRAN")) return(FALSE)
  }

  ## 3. package names must match
  if (resolution$package != candidate$package) return(FALSE)

  ## 4. version requirements must be satisfied. Otherwise good.
  if (resolution$remote[[1]]$version == "") return(TRUE)

  version_satisfies(
    candidate$version,
    resolution$remote[[1]]$atleast,
    resolution$remote[[1]]$version
  )
}

## ----------------------------------------------------------------------
## Internal functions

type_cran_resolve_current <- function(remote, direct, config, cache,
                                      dependencies) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

type_cran_resolve_version <- function(remote, direct, config,
                                      crancache, dependencies) {
  TODO
}
