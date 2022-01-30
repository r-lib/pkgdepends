
## ------------------------------------------------------------------------
## API

#' @importFrom stats na.omit
#' @importFrom desc desc_get_deps

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
  versions <- if ("type" %in% names(remote)) {
    remote$version
  } else  {
    vcapply(remote, "[[", "version")
  }

  if (all(versions %in% c("", "current"))) {
    type_cran_resolve_current(remote, direct, config, cache, dependencies)
  } else {
    type_cran_resolve_version(remote, direct, config, cache, dependencies)
  }
}

download_remote_cran <- function(resolution, target, target_tree, config,
                                 cache, which, on_progress) {

  # if we don't have a shain the metadata, then we need to ping.
  # otherwise we can use the cache.
  download_ping_if_no_sha(resolution, target, config, cache,
                          on_progress)
}

satisfy_remote_cran <- function(resolution, candidate, config, ...) {

  ## 1. candidate must be a cran, standard or installed ref
  if (!candidate$type %in% c("cran", "standard", "installed")) {
    return(structure(
      FALSE, reason = "Type must be 'cran', 'standard' or 'installed'"))
  }

  ## 2. installed refs must be from CRAN
  if (candidate$type == "installed") {
    repotype <- candidate$extra[[1]]$repotype
    if (! identical(repotype, "cran")) {
      return(structure(FALSE, reason = "Installed package not from CRAN"))
    }
  }

  ## 3. package names must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 4. installed package must not be older for direct refs
  if (resolution$direct) {
    if (candidate$type == "installed" &&
        package_version(resolution$version) > candidate$version) {
      return(structure(FALSE, reason = "Direct ref needs update"))
    }
  }

  ## 5. version requirements must be satisfied. Otherwise good.
  if (resolution$remote[[1]]$version == "") return(TRUE)

  if (!version_satisfies(
         candidate$version,
         resolution$remote[[1]]$atleast,
         resolution$remote[[1]]$version)) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}

installedok_remote_cran <- function(installed, solution, config, ...) {

  if (solution$platform != "source") {
    # Binary packages are simple. We need to match `$build` to make sure
    # that we are installing the same build.
    identical(installed$package, solution$package) &&
      identical(installed$version, solution$version) &&
      (identical(installed[["platform"]], solution[["platform"]]) ||
       identical(installed[["platform"]], "*")) &&
      identical(installed[["built"]], solution[["built"]])

  } else {
    # Source packages are different, because only the installed one has a
    # 'Built' field. So we require `Repository == "CRAN"` for these.
    identical(installed$package, solution$package) &&
      identical(installed$version, solution$version) &&
      identical(installed[["repository"]], "CRAN")
  }
}

## ----------------------------------------------------------------------
## Internal functions

type_cran_resolve_current <- function(remote, direct, config, cache,
                                      dependencies) {
  resolve_from_metadata(remote, direct, config, cache, dependencies)
}

type_cran_resolve_version <- function(remote, direct, config,
                                      crancache, dependencies) {
  stop("Versioned CRAN packages are not implemented yet")
}
