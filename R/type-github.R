
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
           dependencies = dependencies[[2 - direct]])$
    then(type_github_make_resolution)
}

download_remote_github <- function(resolution, target, config, cache,
                                   progress_bar) {

  ## A GitHub package needs to be built, from the downloaded repo
  ## If we are downloading a solution, then we skip building the vignettes,
  ## because these will be built later by pkginstall.
  ##
  ## We cache both the downloaded repo snapshot and the built package in
  ## the package cache. So this is how we go:
  ##
  ## 1. If there is a built package in the cache (including vignettes
  ##    if they are needed), then we just use that.
  ## 2. If there is a repo snapshot in the cache, we build an R package
  ##    from it. (Add also add it to the cache.)
  ## 3. Otherwise we download the repo, add it to the cache, build the
  ##    R package, and add that to the cache as well.

  package <- resolution$package
  sha <- resolution$extra[[1]]$sha
  need_vignettes <- ! inherits(resolution, "remotes_solution")

  ## 1. Check if we have a built package in the cache. We don not check the
  ## ref or the type, so the package could have been built from a local
  ## ref or from another repo. As long as the sha is the same, we are
  ## fine. If we don't require vignetted, then a package with or without
  ## vignettes is fine.

  hit <- cache$package$copy_to(
    target, package = package, sha = sha, built = TRUE,
    .list = c(if (need_vignettes) vignettes = TRUE))
  if (nrow(hit)) return("Had")

  ## 2. Check if we have a repo snapshot in the cache.

  target_zip <- sub("\\.tar\\.gz$", ".zip", target)
  rel_target <- resolution$target
  subdir <- resolution$remote[[1]]$subdir
  hit <- cache$package$copy_to(
    target_zip, package = package, sha = sha, built = FALSE)
  if (nrow(hit)) {
    return(type_github_build_package(target_zip, target, rel_target, subdir,
                                     package, sha, need_vignettes, cache))
  }

  ## 3. Need to download the repo

  urls <- resolution$sources[[1]]
  rel_zip <- sub("\\.tar\\.gz$", ".zip", rel_target)
  type_github_download_repo(urls, target_zip, rel_zip, sha, package, cache)$
    then(function() {
      type_github_build_package(target_zip, target, rel_target, subdir,
                                package, sha, need_vignettes, cache)
    })
}

type_github_build_package <- function(repo_zip, target, rel_target, subdir,
                                      package, sha, vignettes, cache) {
  mkdirp(tmpdir <- tempfile())
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  zipfile <- file.path(tmpdir, basename(repo_zip))
  file.copy(repo_zip, zipfile)

  pkgdir <- file.path(tmpdir, unzip(zipfile))[1]
  if (!nzchar(subdir)) pkgdir <- file.path(pkgdir, subdir)
  pkgfile <- build_package(
    pkgdir, build_args = list(vignettes = vignettes))

  file.copy(pkgfile, target)
  cache$package$add(
    target, rel_target, package = package, sha = sha, built = TRUE,
    vignettes = vignettes)
  "Built"
}

type_github_download_repo <- function(urls, repo_zip, rel_zip, sha,
                                      package, cache) {
  download_file(urls, repo_zip)$
    then(function() {
      cache$package$add(
        repo_zip, rel_zip, package = package, sha = sha, built = FALSE)
      "Got"
    })
}

## ----------------------------------------------------------------------

satisfy_remote_github <- function(resolution, candidate,
                                    config, ...) {

  ## 1. package name must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 1. installed ref is good, if it has the same sha
  if (candidate$type == "installed") {
    dsc <- candidate$extra[[1]]$description
    sha1 <- if (!is.null(dsc)) dsc$get("RemoteSha")[[1]]
    sha2 <- resolution$extra[[1]]$sha
    ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
    if (!ok) {
      return(structure(FALSE, reason = "Installed package sha mismatch"))
    } else {
      return(TRUE)
    }
  }

  ## 2. other refs are also good, as long as they have the same sha
  sha1 <- candidate$extra[[1]]$sha
  sha2 <- resolution$extra[[1]]$sha
  ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
  if (!ok) {
    return(structure(FALSE, reason = "Candidate package sha mismatch"))
  } else {
    return(TRUE)
  }
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
      desc(text = rawToChar(resp$content))
    })
}

## Returns a deferred value

type_github_get_github_commit_sha <- function(rem) {
  commit_url <- type_github_get_github_commit_url(rem)
  http_get(commit_url, headers = type_github_get_github_headers())$
    then(http_stop_for_status)$
    then(function(resp) {
      cdata <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      cdata$sha
    })
}

type_github_make_resolution <- function(data) {

  deps <- resolve_ref_deps(data$desc$get_deps(), data$desc$get("Remotes"))

  sha <- data$sha
  username <- data$remote$username
  repo <- data$remote$repo
  subdir <- data$remote$subdir %|z|% NULL
  commitish <- data$remote$commitish %|z|% NULL
  pull <- data$remote$pull %|z|% NULL
  release <- data$remote$release %|z|% NULL
  package <- data$desc$get_field("Package")
  version <- data$desc$get_field("Version")
  dependencies <- data$dependencies
  unknown <- deps$ref[deps$type %in% dependencies]
  unknown <- setdiff(unknown, c(base_packages(), "R"))

  list(
    ref = data$remote$ref,
    type = data$remote$type,
    direct = data$direct,
    status = "OK",
    package = package,
    version = version,
    license = data$desc$get_field("License", NA_character_),
    sources = glue(
      "https://api.github.com/repos/{username}/{repo}/zipball/{sha}"),
    target = glue("src/contrib/{package}_{version}_{sha}.tar.gz"),
    remote = list(data$remote),
    deps = list(deps),
    unknown_deps = unknown,
    extra = list(list(sha = sha))
  )
}
