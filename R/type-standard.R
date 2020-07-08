
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

download_remote_standard <- function(resolution, target, target_tree,
                                     config, cache, which, on_progress) {

  download_ping_if_no_sha(resolution, target, config, cache,
                          on_progress)
}

satisfy_remote_standard <- function(resolution, candidate, config, ...) {

  ## A standard ref is special, in that any ref source can satisfy it,
  ## as long as the package name is the same, and the version
  ## requirements are satisfied.

  ## 1. package name must be the same
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 2. if this is a direct ref, then it has to be a CRAN or
  ## bioc package. If the candidate is an installed package, we
  ## need to check where it was installed from.
  if (resolution$direct) {
    if (candidate$type == "installed") {
      type <- candidate$extra[[1]]$repotype %||% "unknown"
    } else {
      type <- candidate$type
    }
    if (!type %in% c("cran", "bioc", "standard")) {
      return(structure(FALSE, reason = "User requested CRAN package"))
    }
  }

  ## 3. version requirements must be satisfied
  version <- tryCatch(resolution$remote[[1]]$version, error = function(e) "")
  if (version == "") return(TRUE)

  if (!version_satisfies(
         candidate$version,
         resolution$remote[[1]]$atleast,
         version)) {
    return(structure(FALSE, reason = "Insufficient version"))
  }

  TRUE
}
