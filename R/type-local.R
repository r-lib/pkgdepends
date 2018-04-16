
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

  sources <- paste0("file://", normalizePath(remote$path))
  resolve_from_description(remote$path, sources, remote, direct, config,
                           cache, dependencies)
}

download_remote_local <- function(resolution, config, mode,
                                  ..., cache, progress_bar) {
  tryCatch({
    files <- get_files(resolution)[[1]]
    target_file <- file.path(config$cache_dir, files$target)
    mkdirp(dirname(target_file))
    if (! file.copy(files$source, target_file)) stop("No local file found")
    progress_bar$update(count = 1, cached = 1)
    async_constant(list(make_dl_status("Had", files$source, target_file,
                                       bytes = file.size(target_file))))
  }, error = function(err) {
    async_constant(list(make_dl_status("Failed", files$source,
                                       target_file, error = err)))
  })
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
