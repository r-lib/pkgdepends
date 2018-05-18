
## ------------------------------------------------------------------------
## API

#' @importFrom desc desc

parse_remote_installed <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_installed_rx())

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "installed"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_installed <- function(remote, direct, config,
                                     cache, dependencies, ...) {

  path <- file.path(remote$library, remote$package)
  sources <- character()
  deps <- setdiff(dependencies[[2 - direct]], "LinkingTo")
  resolve_from_description(path, sources, remote, direct, config,
                           cache, deps)
}

download_remote_installed <- function(resolution, target, config, cache,
                                      on_progress) {
  "Had"
}

satisfy_remote_installed <- function(resolution, candidate,
                                     config, ...) {
    TRUE
  }

## ----------------------------------------------------------------------
## Internal functions

type_installed_rx <- function() {
  paste0(
    "^",
    "(?:installed::)?",
    "(?<library>.*)/",
    "(?<package>", package_name_rx(), ")",
    "$"
  )
}
