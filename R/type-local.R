
### -----------------------------------------------------------------------
### API

parse_remote_local <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, type_local_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "local"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

resolve_remote_local <- function(remote, direct, config, cache,
                                 dependencies, ...) {

  sources <- paste0("file://", normalizePath(remote$path, mustWork = FALSE))
  resolve_from_description(remote$path, sources, remote, direct,
                           config, cache, dependencies[[2 - direct]])
}

download_remote_local <- function(resolution, target, config, cache,
                                  on_progress) {

  source_file <- sub("^file://",  "",  resolution$sources[[1]])
  if (! file.copy(source_file, target, overwrite =  TRUE)) {
    stop("No local file found")
  }
  "Had"
}

satisfy_remote_local <- function(resolution, candidate, config, ...) {
    ## TODO: we can probably do better than this
    FALSE
  }

## ----------------------------------------------------------------------
## Internal functions

type_local_rx <- function() {
  paste0(
    "^",
    "(?:local::)",
    "(?<path>.*)",
    "$"
  )
}
