
### -----------------------------------------------------------------------
### API

#' @importFrom rematch2 re_match
#' @importFrom jsonlite fromJSON
#' @importFrom desc desc
#' @importFrom glue glue
#' @export

parse_remote.remote_specs_github <- function(specs, config, ...) {

  pds <- re_match(specs, github_rx())

  pds$ref <- pds$.text
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "github"
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$repo)
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i,])
  )
}

#' @export

resolve_remote.remote_ref_github <- function(remote, config, ...,
                                             cache) {
  dependencies <- config$dependencies

  ## Get the DESCRIPTION data, and the SHA we need
  desc <- type_github_get_github_description_data(remote)
  sha <- type_github_get_github_commit_sha(remote)
  when_all(desc = desc, sha = sha, remote = remote,
           dependencies = dependencies)$
    then(type_github_make_resolution)
}

#' @export

download_remote.remote_resolution_github <- function(resolution, config,
                                                     ..., cache) {

  cache_dir <- config$cache_dir

  ref <- resolution$remote$ref

  if (length(resolution$files) != 1) {
    stop("Invalid `files` vector, should be length one.")
  }
  files <- resolution$files[[1]]

  target_file <- file.path(cache_dir, files$target)
  cached_zip <- sub("\\.tar\\.gz$", ".zip", target_file)
  mkdirp(dirname(target_file))
  subdir <- resolution$remote$subdir
  url <- files$source

  if (is_valid_package(target_file)) {
    status <- make_dl_status("Had", files$source, target_file,
                             bytes = file.size(target_file))
    async_constant(list(status))

  } else if (file.exists(cached_zip)) {
    type_github_build_github_package(cached_zip, target_file, subdir)
    status <- make_dl_status("Had", url, target_file,
                             bytes = file.size(target_file))
    async_constant(list(status))

  } else {
    download_file(url, cached_zip)$
      then(function() {
        type_github_build_github_package(cached_zip, target_file, subdir)
        list(make_dl_status("Got", url, target_file,
                            bytes = file.size(target_file)))
      })$
      catch(function(err) {
        list(make_dl_status("Failed", url, target_file,
                            error = err))
      })
  }
}

## ----------------------------------------------------------------------

#' @export

satisfies_remote.remote_resolution_github <- function(resolution, candidate,
                                                      config, ...) {

  ## 1. package name must match
  if (resolution$remote$package != candidate$remote$package) return(FALSE)


  ## 1. installed ref is good, if it has the same same ref
  if (inherits(candidate, "remote_resolution_installed")) {
    dsc <- candidate$resolution$description
    sha1 <- dsc$get("RemoteSha")[[1]]
    sha2 <- resolution$remote$sha
    return(is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2))
  }

  ## 2. other refs are also good, as long as they have the same sha
  sha1 <- candidate$remote$sha
  sha2 <- resolution$remote$sha
  return(is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2))
}

## ----------------------------------------------------------------------
## Internal functions

type_github_get_github_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")

  if (nzchar(token <- Sys.getenv("GITHUB_TOKEN")) ||
      nzchar(token <- Sys.getenv("GITHUB_PAT"))) {
    headers <- c(headers, c("Authorization" = paste("token", token)))
  }
  headers
}

type_github_get_github_description_url <- function(rem) {
  glue(
    "https://raw.githubusercontent.com/{rem$username}",
    "/{rem$repo}/{commitish}/{rem$subdir}/DESCRIPTION",
    commitish = if (nzchar(rem$commitish)) rem$commitish else "master"
  )
}

type_github_get_github_commit_url <- function(rem) {
  glue(
    "https://api.github.com/repos/{rem$username}/{rem$repo}",
    "/git/refs/heads/{commitish}",
    commitish = if (nzchar(rem$commitish)) rem$commitish else "master"
  )
}

## Returns a deferred value

type_github_get_github_description_data <- function(rem) {
  description_url <- type_github_get_github_description_url(rem)
  http_get(description_url, headers = type_github_get_github_headers())$
    then(function(resp) {
      write_bin_atomic(resp$content, tmp <- tempfile())
      dsc <- desc(tmp)
      gx <- function(e) unname(str_trim(dsc$get(e)))
      list(
        error   = NULL,
        package = gx("Package"),
        version = gx("Version"),
        remotes = gx("Remotes"),
        deps = dsc$get_deps()
      )
    })$
    catch(function(err) {
      list(
        error   = err,
        package = NA_character_,
        version = NA_character_,
        remotes = NA_character_,
        deps    = NA_character_
      )
    })
}

## Returns a deferred value

#' @importFrom async http_stop_for_status

type_github_get_github_commit_sha <- function(rem) {
  commit_url <- type_github_get_github_commit_url(rem)
  http_get(commit_url, headers = type_github_get_github_headers())$
    then(http_stop_for_status)$
    then(function(resp) {
      cdata <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      list(error = NULL, sha = cdata$object$sha)
    })$
    catch(function(err) {
      list(error = err, sha = NA_character_)
    })
}

type_github_build_github_package <- function(source, target, subdir) {
  mkdirp(zipdir <- tempfile())
  on.exit(unlink(zipdir, recursive = TRUE), add = TRUE)
  zipfile <- file.path(zipdir, basename(source))
  file.copy(source, zipfile)

  pkgdir <- file.path(zipdir, unzip(zipfile))[1]
  if (nzchar(subdir)) pkgdir <- file.path(pkgdir, subdir)
  pkgfile <- build_package(pkgdir)

  file.copy(pkgfile, target)
}

type_github_make_resolution <- function(data) {

  deps <- if (is.null(data$error)) {
    resolve_ref_deps(
      data$desc$deps, data$desc$remotes, data$dependencies)
  } else {
    NA_character_
  }

  sha <- data$sha$sha
  username <- data$remote$username
  repo <- data$remote$repo
  package <- data$desc$package
  version <- data$desc$version
  desc_err <- data$desc$error
  sha_err <- data$sha$error

  files <- list(
    source = glue(
      "https://api.github.com/repos/{username}/{repo}/zipball/{sha}"),
    target = glue("src/contrib/{package}_{version}_{sha}.tar.gz"),
    platform = "source",
    rversion = "*",
    dir = "src/contrib",
    package = package,
    version = version,
    deps = deps,
    status = if (is.null(desc_err %||% sha_err)) "OK" else "FAILED",
    error = list(desc = desc_err, sha = sha_err)
  )

  data$remote$sha <- sha

  structure(
    list(files = list(files), remote = data$remote, status = files$status),
    class = c("remote_resolution_github", "remote_resolution")
  )
}
