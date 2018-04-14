
### -----------------------------------------------------------------------
### API

#' @importFrom rematch2 re_match
#' @importFrom jsonlite fromJSON
#' @importFrom desc desc
#' @importFrom glue glue

parse_remote_github <- function(specs, config, ...) {

  pds <- re_match(specs, github_rx())
  if (any(unk <- is.na(pds$.match))) {
    pds[unk] <- re_match(specs[unk], github_url_rx())
    pds[unk, "subdir"] <- ""
  }

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

resolve_remote_github <- function(remote, direct, config, cache,
                                  dependencies, ...) {

  force(direct); force(dependencies)
  ## Get the DESCRIPTION data, and the SHA we need
  desc <- type_github_get_github_description_data(remote)
  sha <- type_github_get_github_commit_sha(remote)
  when_all(desc = desc, sha = sha, remote = remote, direct = direct,
           dependencies = dependencies)$
    then(type_github_make_resolution)
}

download_remote_github <- function(resolution, config,
                                   mode, ..., cache,
                                   progress_bar) {

  cache_dir <- config$cache_dir

  ref <- get_ref(resolution)

  if (num_files(resolution) != 1) {
    stop("Invalid `files` vector, should be length one.")
  }
  files <- get_files(resolution)[[1]]

  target_file <- file.path(cache_dir, files$target)
  cached_zip <- sub("\\.tar\\.gz$", ".zip", target_file)
  mkdirp(dirname(target_file))
  remote <- get_remote(resolution)
  subdir <- remote$subdir
  url <- files$source
  vignettes <- (mode == "resolution")
  metadata <- list(type = "github", ref = ref, sha = remote$sha,
                   package = remote$package, version = files$version,
                   platform = "source", vignettes = vignettes)

  if (is_valid_package(target_file)) {
    progress_bar$update(count = 1, cached = 1)
    status <- make_dl_status("Had", url, target_file,
                             bytes = file.size(target_file))
    async_constant(list(status))

  } else if (file.exists(cached_zip)) {
    progress_bar$alert(class = "alert-start",
                       "Building {basename(target_file)}")
    dsc <- type_github_build_github_package(cached_zip, target_file, subdir,
                                            vignettes = vignettes)
    progress_bar$update(count = 1, cached = 1)
    ## Add built package to the cache
    try(
      cache$package_cache$add(target_file, path = files$target, url = url,
                              etag = NA_character_, .list = metadata),
      silent = TRUE
    )
    status <- make_dl_status("Had", url, target_file,
                             bytes = file.size(target_file))
    async_constant(list(status))

  } else {

    ## Try to get the built package from the cache
    hit <- cache$package_cache$copy_to(target_file, .list = metadata)
    if (nrow(hit) >= 1) {
      res <- make_dl_status(
        "Had", url, target_file, bytes = file.size(target_file))
      return(async_constant(list(res)))
    }

    download_file(url, cached_zip, progress_bar = progress_bar)$
      then(function() {
        ## Build source package from zip (R CMD build)
        progress_bar$alert(class = "alert-start",
                           "Building {basename(target_file)}")
        dsc <- type_github_build_github_package(cached_zip, target_file, subdir,
                                                vignettes = vignettes)
        ## Add built package to the cache
        try(
          cache$package_cache$add(target_file, path = files$target, url = url,
                                  etag = NA_character_, .list = metadata),
          silent = TRUE
        )
        list(make_dl_status("Got", url, target_file,
                            bytes = file.size(target_file)))
      })$
      catch(error = function(err) {
        list(make_dl_status("Failed", url, target_file,
                            error = err))
      })
  }
}

## ----------------------------------------------------------------------

satisfy_remote_github <- function(resolution, candidate,
                                    config, ...) {

  ## 1. package name must match
  if (get_remote(resolution)$package != get_remote(candidate)$package) {
    return(FALSE)
  }

  ## 1. installed ref is good, if it has the same sha
  if (inherits(candidate, "remote_resolution_installed")) {
    dsc <- get_remote(candidate)$description
    sha1 <- dsc$get("RemoteSha")[[1]]
    sha2 <- get_remote(resolution)$sha
    return(is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2))
  }

  ## 2. other refs are also good, as long as they have the same sha
  sha1 <- get_remote(candidate)$sha
  sha2 <- get_remote(resolution)$sha
  return(is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2))
}

## ----------------------------------------------------------------------
## Internal functions

type_github_get_github_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")

  if (nzchar(token <- Sys.getenv("GITHUB_TOKEN",
                                 Sys.getenv("GITHUB_PAT")))) {
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
    "/git/trees/{commitish}",
    commitish = if (nzchar(rem$commitish)) rem$commitish else "master"
  )
}

## Returns a deferred value

type_github_get_github_description_data <- function(rem) {
  description_url <- type_github_get_github_description_url(rem)
  http_get(description_url, headers = type_github_get_github_headers())$
    then(http_stop_for_status)$
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
    catch(error = function(err) {
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

type_github_get_github_commit_sha <- function(rem) {
  commit_url <- type_github_get_github_commit_url(rem)
  http_get(commit_url, headers = type_github_get_github_headers())$
    then(http_stop_for_status)$
    then(function(resp) {
      cdata <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      list(error = NULL, sha = cdata$sha)
    })$
    catch(error = function(err) {
      list(error = err, sha = NA_character_)
    })
}

#' @importFrom desc desc

type_github_build_github_package <- function(source, target, subdir,
                                             vignettes) {
  mkdirp(zipdir <- tempfile())
  on.exit(unlink(zipdir, recursive = TRUE), add = TRUE)
  zipfile <- file.path(zipdir, basename(source))
  file.copy(source, zipfile)

  pkgdir <- file.path(zipdir, unzip(zipfile))[1]
  if (nzchar(subdir)) pkgdir <- file.path(pkgdir, subdir)
  pkgfile <- build_package(
    pkgdir, build_args = list(vignettes = vignettes))

  file.copy(pkgfile, target)
  desc(target)
}

type_github_make_resolution <- function(data) {

  deps <- if (is.null(data$desc$error)) {
    resolve_ref_deps(
      data$desc$deps, data$desc$remotes, data$dependencies)
  } else {
    NA_character_
  }

  sha <- data$sha$sha
  username <- data$remote$username
  repo <- data$remote$repo
  subdir <- data$remote$subdir %|z|% NULL
  commitish <- data$remote$commitish %|z|% NULL
  pull <- data$remote$pull %|z|% NULL
  release <- data$remote$release %|z|% NULL
  package <- data$remote$package
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
    needs_compilation = NA_character_,
    status = if (is.null(desc_err %||% sha_err)) "OK" else "FAILED",
    error = list(desc = desc_err, sha = sha_err)
  )

  files$metadata <- c(
    RemoteOriginalRef = data$remote$ref,
    RemoteType = "github",
    RemotePkgType = "source",
    RemoteHost = "api.github.com",  # TODO: update if others are supported
    RemoteRepo = repo,
    RemoteUsername = username,
    RemoteSubdir = subdir,
    RemoteRef = commitish,
    RemotePull = pull,
    RemoteRelease = release,
    RemoteSha = sha
  )

  data$remote$sha <- sha

  structure(
    list(
      files = list(files), direct = data$direct, remote = data$remote,
      status = files$status),
    class = c("remote_resolution_github", "remote_resolution")
  )
}
