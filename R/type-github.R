
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
  when_all(desc = desc, sha = sha)$
    then(function(data) {
      deps <- resolve_ref_deps(
        data$desc$deps, data$desc$remotes, dependencies)
      files <- list(
        source = glue(
          "https://api.github.com/repos/{remote$username}/{remote$repo}",
          "/zipball/{data$sha}"),
        target = glue(
          "src/contrib/{data$desc$package}_{data$desc$version}",
          "_{data$sha}.tar.gz"),
        platform = "source",
        rversion = "*",
        dir = "src/contrib",
        package = data$desc$package,
        version = data$desc$version,
        deps = deps,
        status = "OK"
      )

    remote$sha <- sha

    structure(
      list(files = list(files), remote = remote, status = "OK"),
      class = c("remote_resolution_github", "remote_resolution")
    )
  })
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
        error <- if (is.list(err)) err$error %||% err else err
        list(make_dl_status("Failed", url, target_file,
                            error = error))
      })
  }
}

## ----------------------------------------------------------------------

#' @export

satisfies_remote.remote_resolution_github <-
  function(resolution, candidate, config, ...) {
    rrem <- resolution$remote
    crem <- candidate$remote
    if (crem$type == "installed") {
      dsc <- crem$description
      ## If SHA is the same, then they are the same
      if (identical(rrem$sha, dsc$get("RemoteSha")[[1]])) return(TRUE)
      ## Otherwise type, username and repo must match
      if (! identical(dsc$get("RemoteType")[[1]], "github")) return(FALSE)
      if (! identical(dsc$get("RemoteUsername")[[1]], rrem$username)) {
        return(FALSE)
      }
      if (! identical(dsc$get("RemoteRepo")[[1]], rrem$repo)) {
        return(FALSE)
      }
      ## If branch matches as well, we are good. Otherwise not.
      ## If HEAD is specified, then the SHA must match as well
      ccomm <- dsc$get("RemoteRef")
      rcomm <- rrem$commitish
      if (ccomm == rcomm && ccomm != "HEAD") return(TRUE)
      if (ccomm == "master" && rcomm == "") return(TRUE)
      FALSE

    } else if (crem$type == "github") {
      ## If SHA is the same, then they are the same
      if (identical(rrem$sha, crem$sha)) return(TRUE)
      ## Otherwise username and repo must match
      if (! identical(crem$username, rrem$username)) return(FALSE)
      if (! identical(crem$repo, rrem$repo)) return(FALSE)
      ## If branch matches as well, we are good. Otherwise not.
      ## If HEAD is specified, then the SHA must match as well
      ccomm <- crem$commitish
      rcomm <- rrem$commitish
      if (ccomm == rcomm && ccomm != "HEAD") return(TRUE)
      if (ccomm == "master" && rcomm == "") return(TRUE)
      if (ccomm == "" && rcomm == "master") return(TRUE)
      FALSE

    } else {
      FALSE
    }
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
        package = gx("Package"),
        version = gx("Version"),
        remotes = gx("Remotes"),
        deps = dsc$get_deps()
      )
    })
}

## Returns a deferred value

type_github_get_github_commit_sha <- function(rem) {
  commit_url <- type_github_get_github_commit_url(rem)
  http_get(commit_url, headers = type_github_get_github_headers())$
    then(function(resp) {
      cdata <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      cdata$sha
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
