
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

  tryCatch({
    dsc <- desc(file = remote$path)

    deps <- resolve_ref_deps(
      dsc$get_deps(), dsc$get("Remotes")[[1]], dependencies)

    rversion <- tryCatch(
      get_minor_r_version(dsc$get_built()$R),
      error = function(e) "*"
    )

    platform <- tryCatch(
      dsc$get_built()$Platform %|z|% "source",
      error = function(e) "source"
    )

    nc <- dsc$get_field("NeedsCompilation", NA)
    if  (!is.na(nc)) nc <- tolower(nc) %in% c("true", "yes")

    remote$description <- dsc

    list(
      ref = remote$ref,
      type = remote$type,
      status = "OK",
      package = dsc$get_field("Package"),
      version = dsc$get_field("Version"),
      license = dsc$get_field("License", NA_character_),
      needscompilation = nc,
      md5sum = dsc$get_field("MD5sum", NA_character_),
      built = dsc$get_field("Built", NA_character_),
      platform = platform,
      rversion = rversion,
      deps = list(deps),
      sources = list(paste0("file://", normalizePath(remote$path))),
      remote = list(remote),
      unknown_deps = setdiff(deps$ref, "R")
    )
  }, error = function(err) {

    list(
      ref = remote$ref,
      type = remote$type,
      status = "FAILED",
      error = list(err),
      package = dsc$get_field("Package", NA_character_),
      version = dsc$get_field("Version", NA_character_),
      license = dsc$get_field("License", NA_character_),
      sources = list(NA_character_)
    )
  })
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
