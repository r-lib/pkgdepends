
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

  # prepare a default resolution
  sources <- paste0("file://", normalizePath(remote$path, mustWork = FALSE))
  res <- resolve_from_description(remote$path, sources, remote, direct,
                           config, cache, dependencies[[2 - direct]])

  # save the source as the remote url
  res$metadata[["RemoteUrl"]] <- sources[[1]]

  # collect the list of local package files
  pkg_files <- list.files(
    remote$path[[1]], 
    full.names = TRUE, 
    recursive = TRUE,
    # this will exclude hidden files, but maybe
    # some package rely on them? 
    all.files = FALSE
  )

  # save the md5sum
  # TODO: make one sum out of it?
  # TODO: use sha256 and digest?
  if (length(pkg_files) > 0) {
    md5sum <- paste0(tools::md5sum(pkg_files), collapse = ".")
    res$extra[[1]][["remotemd5sum"]] <- md5sum
    res$metadata[["RemoteMD5Sum"]] <- md5sum
  }

  res
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
  ## 1. package name must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 2. installed from the same identical local source is good
  if (candidate$type == "installed") {
    want_reinst <- is_true_param(resolution$params[[1]], "reinstall")
    if (want_reinst) {
      return(structure(FALSE, reason = "Re-install requested"))
    }
    # check if the file path matches
    candidate_url <- candidate$extra[[1]][["remoteurl"]] %||% NA_character_
    local_url <- resolution$sources[[1]]
    if (!identical(candidate_url, local_url)) {
      return(structure(FALSE, reason = "Installed package path mismatch"))
    }

    # check if the md5sum maches
    candidate_md5 <- candidate$extra[[1]][["remotemd5sum"]] %||% NA_character_
    local_md5 <- resolution$extra[[1]][["remotemd5sum"]] %||% NA_character_
    if (!identical(candidate_md5, local_md5)) {
      return(structure(FALSE, reason = "Installed package md5sum mismatch"))
    } 

    # it's good!
    return(TRUE)
  }

  ## 3. no other candidate works
  FALSE
}

installedok_remote_local <- function(installed, solution, config, ...) {
  ## TODO: we can probably do better than this
  FALSE
}
