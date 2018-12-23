
parse_remote_deps <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_deps_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "deps"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_deps <- function(remote, direct, config, cache,
                                     dependencies, ...) {

  ret <- resolve_remote_local(remote, direct, config, cache,
                              dependencies, ...)
  ret$sources <- list(character())
  ret
}

download_remote_deps <- function(resolution, target, config, cache,
                                  which, on_progress) {
  ## Nothing to do here
  "Had"
}

satisfy_remote_deps <- function(resolution, candidate, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}

## ----------------------------------------------------------------------
## Internal functions

type_deps_rx <- function() {
  paste0(
    "^",
    "(?:deps::)",
    "(?<path>.*)",
    "$"
  )
}
