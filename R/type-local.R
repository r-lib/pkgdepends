
### -----------------------------------------------------------------------
### API

parse_remote_local <- function(specs, config, ...) {
  parsed_specs <- re_match(specs, local_rx())
  parsed_specs$ref <- paste0("local::", parsed_specs$path)
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

download_remote_local <- function(resolution, target, target_tree, config,
                                  cache, which, on_progress) {

  source_file <- sub("^file://",  "",  resolution$sources[[1]])
  isdir <- file.info(source_file)$isdir
  if (is.na(isdir)) stop("Local file not found")

  if (isdir) {
    unlink(target_tree, recursive = TRUE)
    mkdirp(target_tree)
    if (! file.copy(source_file, target_tree, recursive = TRUE)) {
      stop("No local file found")
    }
  } else {
    if (! file.copy(source_file, target, overwrite = TRUE)) {
      stop("No local file found")
    }
  }

  "Got"
}

satisfy_remote_local <- function(resolution, candidate, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}

installedok_remote_local <- function(installed, solution, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}
