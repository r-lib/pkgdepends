
parse_remote_git <- function(specs, config, ...) {
  pds <- re_match(specs, git_rx())
  pds$ref <- pds$.text
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "git"
  pds$dotgit <- sub("^.*[.]git$", ".git", pds$repo)
  pds$repo <- sub("[.]git$", "", pds$repo)
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$repo)
  pds$url <- paste0(pds$protocol, "://", pds$host, pds$path, pds$repo, pds$dotgit)
  pds$commitish[pds$commitish == ""] <- "HEAD"
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i, ])
  )
}

resolve_remote_git <- function(remote, direct, config, cache,
                               dependencies, ...) {
  type_git_get_data(remote)$
    then(function(resp) {
      data <- list(
        desc = resp$description,
        sha = resp$sha,
        remote = remote,
        direct = direct,
        dependencies = dependencies[[2 - direct]]
      )
      type_git_make_resolution(data)
    })
}

download_remote_git <- function(resolution, target, target_tree,
                                config, cache, which, on_progress) {

  # TODO: cache
  # TODO: auth
  url <- resolution$remote[[1]]$url
  sha <- resolution$metadata[[1]][["RemoteSha"]]
  pkgdir <- file.path(target_tree, resolution$package)
  # TODO: remove pkgdir if the download fails?
  mkdirp(pkgdir)
  async_git_download_repo(url, ref = sha, output = pkgdir)$
    then(function() {
      "Got"
    })
}

satisfy_remote_git <- function(resolution, candidate,
                               config, ...) {
  # TODO
  FALSE
}

installedok_remote_git <- function(installed, solution, config, ...) {
  # TODO
  FALSE
}

# -------------------------------------------------------------------------

git_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Remote type
    "(?:git::)",
    "(?:(?<protocol>[^/]*)://)?",
    "(?<host>[^/]+)",
    "(?<path>[^@]*/)",
    "(?<repo>[^/@]*)",
    "(?:@(?<commitish>.*))?"
  )
}

# TODO: auth

type_git_get_data <- function(remote) {
  remote
  sha <- NULL
  dsc <- NULL
  async_git_list_files(remote$url, remote$commitish)$
    then(function(files) {
      sha <<- files$sha
      desc_idx <- which(files$files$path == "DESCRIPTION")
      if (length(desc_idx) == 0) {
        throw(pkg_error(
          "Could not find {.path DESCRIPTION} in git repo at {.url {remote$url}}."
        ))
      }
      if (files$files$type[desc_idx] != "blob") {
        throw(pkg_error(
          "{.path DESCRIPTION} is a directory in git repo at {.url {remote$url}}."
        ))
      }
      files$files$hash[desc_idx]
    })$
    catch(error = function(err) {
      TODO
    })$
    then(function(desc_hash) {
      async_git_download_file(remote$url, desc_hash, output = NULL)
    })$
    catch(error = function(err) {
      TODO
    })$
    then(function(desc_dl) {
      dsc <<- desc::desc(text = rawToChar(desc_dl$raw))
    })$
    catch(error = function(err) {
      TODO
    })$
    then(function() {
      list(sha = sha, description = dsc)
    })
}

type_git_make_resolution <- function(data) {
  deps <- resolve_ref_deps(
    data$desc$get_deps(),
    data$desc$get("Remotes"),
    data$desc$get(extra_config_fields(data$desc$fields()))
  )

  sha <- data$sha
  sha7 <- substr(sha, 1, 7)
  commitish <- data$remote$commitish %|z|% NULL
  package <- data$desc$get_field("Package")
  version <- data$desc$get_field("Version")
  dependencies <- data$dependencies
  unknown <- deps$ref[deps$type %in% dependencies]
  unknown <- setdiff(unknown, c(base_packages(), "R"))

  meta <- c(
    RemoteType = "git",
    RemoteUrl = data$remote$url,
    RemotePkgRef = data$remote$ref,
    RemoteRef = commitish %||% "HEAD",
    RemoteSha = sha
  )

  list(
    ref = data$remote$ref,
    type = data$remote$type,
    direct = data$direct,
    status = "OK",
    package = package,
    version = version,
    license = data$desc$get_field("License", NA_character_),
    sources = data$remote$url,
    target = unclass(glue("src/contrib/{package}_{version}_git_{sha7}")),
    remote = list(data$remote),
    deps = list(deps),
    unknown_deps = unknown,
    extra = list(list(remotesha = sha)),
    metadata = meta,
    params = data$remote$params,
    sysreqs = data$desc$get_field("SystemRequirements", "")
  )
}
