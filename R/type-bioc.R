
## ------------------------------------------------------------------------
## API

#' @importFrom glue glue_data

parse_remote_bioc <- function(specs, config, ...) {

  ## BioC is the same as CRAN, except for cran:: -> bioc::
  parsed_specs <- re_match(specs, standard_rx("bioc"))
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "bioc"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_bioc <- function(remote, direct, config, cache,
                                dependencies, progress_bar, ...) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

download_remote_bioc <- function(resolution, config, mode,
                                 ..., cache, progress_bar) {
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

satisfy_remote_bioc <- function(resolution, candidate,
                                config, ...) {

  ## 1. candidate must be a bioc, standard or installed ref
  if (! inherits(candidate, "remote_resolution_bioc") &&
      ! inherits(candidate, "remote_resolution_standard") &&
      ! inherits(candidate, "remote_resolution_installed")) {
    return(FALSE)
  }

  ## 2. installed refs must be from bioc
  if (inherits(candidate, "remote_resolution_installed")) {
    dsc <- get_remote(candidate)$description
    if (is.na(dsc$get("biocViews"))) return(FALSE)
  }

  ## 3. package names must match
  if (get_remote(resolution)$package != get_remote(candidate)$package) {
    return(FALSE)
  }

  ## 4. version requirements must be satisfied. Otherwise good.
  if (get_remote(resolution)$version == "") return(TRUE)

  version_satisfies(
    get_files(candidate)[[1]]$version,
    get_remote(resolution)$atleast,
    get_remote(resolution)$version
  )
}

## ----------------------------------------------------------------------
## Internal functions

type_bioc_matching_bioc_version <- function(r_version) {
  if (r_version >= "3.5") {
    "3.7"
  } else if (r_version >= "3.4") {
    "3.6"
  } else if (r_version >= "3.3.0") {
    "3.4"
  } else if (r_version >= "3.2") {
    "3.2"
  } else if (r_version >= "3.1.1") {
    "3.0"
  } else if (r_version == "3.1.0") {
    "2.14"
  } else if (r_version >= "2.15" && r_version <= "2.16") {
    "2.11"
  } else {
    stop("Cannot get matching BioConductor version for ", r_version)
  }
}

type_bioc_get_bioc_repos <- function(r_version) {
  bv <- type_bioc_matching_bioc_version(r_version)
  tmpl <- c(
    BioCsoft  = "https://bioconductor.org/packages/{bv}/bioc",
    BioCann   = "https://bioconductor.org/packages/{bv}/data/annotation",
    BioCexp   = "https://bioconductor.org/packages/{bv}/data/experiment",
    BioCextra = if (package_version(bv) <= 3.5) {
                  "https://bioconductor.org/packages/{bv}/extra"
                }
  )
  list(
    repos = vcapply(tmpl, glue_data, .x = list(bv = bv)),
    version = bv
  )
}
