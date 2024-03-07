
parse_remote_git <- function(specs, config, ...) {
  pds <- re_match(specs, git_rx())
  pds$ref <- pds$.text
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "git"
  pds$dotgit <- ifelse(grepl("[.]git$", pds$repo), ".git",  "")
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

  # This is similar to github

  package <- resolution$package
  sha <- resolution$extra[[1]][["remotesha"]] %||% NA_character_
  need_vignettes <- which == "resolution"
  nocache <- is_true_param(resolution$params[[1]], "nocache")
  source <- is_true_param(resolution$params[[1]], "source")

  # in case there is a leftover tree here
  unlink(target_tree, recursive = TRUE)

  ## 1. Check for a binary package

  if (!nocache && !source) {
    ptfm <- current_r_platform()
    hit <- cache$package$copy_to(
      target, package = package, sha256 = sha, built = TRUE,
      platform = ptfm, rversion = current_r_version(),
      .list = c(if (need_vignettes) c(vignettes = TRUE)))

    if (nrow(hit)) {
      return(paste("Had", ptfm)) # TODO: untested currently
    }
  }

  ## 2. Check if we have a built package in the cache. We do not check the
  ## ref or the type, so the package could have been built from a local
  ## ref or from another repo. As long as the sha is the same, we are
  ## fine. If we don't require vignetted, then a package with or without
  ## vignettes is fine.

  if (!nocache) {
    hit <- cache$package$copy_to(
      target, package = package, sha256 = sha, built = TRUE,
      .list = c(if (need_vignettes) c(vignettes = TRUE)))
    if (nrow(hit)) {
      return("Had")
    }
  }

  ## 3. Check if we have a repo snapshot in the cache.

  rel_target <- resolution$target
  if (!nocache) {
    subdir <- resolution$remote[[1]]$subdir
    hit <- cache$package$copy_to(
      target_tree, package = package, sha256 = sha, built = FALSE)
    if (nrow(hit)) {
      return("Had")
    }
  }

  ## 4. Need to download the repo

  url <- git_auth_url(resolution$remote[[1]])
  sha <- resolution$metadata[[1]][["RemoteSha"]]
  pkgdir <- file.path(target_tree, resolution$package)
  mkdirp(pkgdir)
  async_git_download_repo(url, ref = sha, output = pkgdir)$
    then(function(files) {
      check_git_download_missing_files(
        resolution$remote[[1]],
        files = files,
        output = pkgdir
      )
    })$
    then(function() {
      "Got"
    })
}


#' Check Downloaded git Repo for Missing Files
#'
#' Throws a warning if any files failed to download which are necessary for the
#' building of the package from source. Will ignore the failure to download
#' files outside a specified remote sub-directory or omitted using a
#' `.Rbuildignore` file.
#'
#' @param remote A git remote
#' @param files A named logical vector with names corresponding to relative
#'   file paths to each file in the git repository, and a logical value
#'   indicating whether the file was successfully downloaded.
#' @param output The directory containing downloaded repository files
#'
#' @return NULL, function used for its side-effects
#'

check_git_download_missing_files <- function(remote, files, output) {
  # ignore reporting files outside of subdirectory
  subdir <- remote$subdir %||% ""
  in_subdir <- startsWith(names(files), subdir)
  files <- files | !in_subdir

  # ignore reporting files that would be ignored using `.Rbuildignore`
  ignore_path_parts <- c(output, remote$subdir, ".Rbuildignore")
  ignore_path <- do.call(file.path, as.list(ignore_path_parts))
  if (file.exists(ignore_path)) {
    subdir_path <- character(length(files))
    subdir_path[in_subdir] <- substring(names(files), nchar(subdir) + 1)
    ignore <- readLines(ignore_path, warn = FALSE)
    for (pattern in ignore[nzchar(ignore)]) {
      is_ignored <- grepl(pattern, subdir_path, perl = TRUE, ignore.case = TRUE)
      files <- files | (in_subdir & is_ignored)
    }
  }

  # warn if any files are necessary, but missing
  if (any(!files)) {
    warning(
      "Files were unable to be fetched from the git remote: ",
      paste("'", names(files)[!files], "'", collapse = ", ")
    )
  }

  invisible(files)
}

satisfy_remote_git <- function(resolution, candidate,
                               config, ...) {
  ## 1. package name must match
  if (resolution$package != candidate$package) {
    return(structure(FALSE, reason = "Package names differ"))
  }

  ## 2. installed ref is good, if it has the same sha
  if (candidate$type == "installed") {
    want_reinst <- is_true_param(resolution$params[[1]], "reinstall")
    if (want_reinst) {
      return(structure(FALSE, reason = "Re-install requested"))
    }
    sha1 <- candidate$extra[[1]][["remotesha"]] %||% NA_character_
    sha2 <- resolution$extra[[1]][["remotesha"]] %||% NA_character_
    ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
    if (!ok) {
      return(structure(FALSE, reason = "Installed package sha mismatch"))
    } else {
      return(TRUE)
    }
  }

  ## 3. local packages satisfy a git remote
  ## See https://github.com/r-lib/pkgdepends/issues/229
  if (candidate$type == "local") {
    return (TRUE)
  }

  ## 3. other refs are also good, as long as they have the same sha
  sha1 <- (if (is.list(candidate$extra[[1]]))candidate$extra[[1]][["remotesha"]]) %||% NA_character_
  sha2 <- resolution$extra[[1]][["remotesha"]] %||% NA_character_
  ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
  if (!ok) {
    return(structure(FALSE, reason = "Candidate package sha mismatch"))
  } else {
    return(TRUE)
  }
}

installedok_remote_git <- function(installed, solution, config, ...) {
  identical(installed$package, solution$package) &&
    identical(installed$version, solution$version) &&
    identical(installed[["remotesha"]], solution$metadata[[1]][["RemoteSha"]])
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

git_auth_url <- function(remote) {
  url <- remote$url
  auth <- tryCatch(gitcreds_get(url), error = function(err) NULL)
  if (is.null(auth)) {
    url
  } else {
    paste0(
      remote$protocol,
      "://",
      auth$username,
      ":",
      auth$password,
      "@",
      sub(paste0("^", remote$protocol, "://"), "", remote$url)
    )
  }
}

type_git_get_data <- function(remote) {
  remote
  sha <- NULL
  dsc <- NULL
  auth_url <- git_auth_url(remote)
  desc_path <- if (is.null(remote$subdir) || remote$subdir == "") {
    "DESCRIPTION"
  } else {
    paste0(remote$subdir, "/", "DESCRIPTION")
  }

  async_git_list_files(auth_url, remote$commitish)$
    catch(error = function(err) {
      throw(pkg_error(
        "Failed to download {.path {desc_path}} from git repo at {.url {remote$url}}."
      ), parent = err)
    })$
    then(function(files) {
      sha <<- files$sha
      desc_idx <- which(files$files$path == desc_path)
      if (length(desc_idx) == 0) {
        throw(pkg_error(
          "Could not find {.path {desc_path}} in git repo at {.url {remote$url}}."
        ))
      }
      if (files$files$type[desc_idx] != "blob") {
        throw(pkg_error(
          "{.path {desc_path}} is a directory in git repo at {.url {remote$url}}."
        ))
      }
      files$files$hash[desc_idx]
    })$
    then(function(desc_hash) {
      async_git_download_file(auth_url, desc_hash, output = NULL)$
      catch(error = function(err) {
        throw(pkg_error(
          "Failed to download {.path {desc_path}} from git repo at {.url {remote$url}}."
        ), parent = err)
      })$
      then(function(desc_dl) {
        dsc <<- desc::desc(text = rawToChar(desc_dl$raw))
      })$
      catch(error = function(err) {
        throw(pkg_error(
          "Failed to parse {.path {desc_path}} from git repo at {.url {remote$url}}."
        ), parent = err)
      })
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
    target = unclass(sprintf("src/contrib/%s_%s_git_%s", package, version, sha7)),
    remote = list(data$remote),
    deps = list(deps),
    unknown_deps = unknown,
    extra = list(list(remotesha = sha)),
    metadata = meta,
    params = data$remote$params,
    sysreqs = data$desc$get_field("SystemRequirements", "")
  )
}
