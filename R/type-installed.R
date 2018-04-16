
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
  resolve_from_description(path, sources, remote, direct, config,
                           cache, dependencies[[2 - direct]])
}

download_remote_installed <- function(resolution,
                                      config, mode, ...,
                                      cache) {
  status <- make_dl_status("Had", NA_character_, NA_character_,
                           bytes = NA)
  async_constant(list(status))
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
