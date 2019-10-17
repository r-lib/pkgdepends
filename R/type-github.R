
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
  desc <- type_github_get_description_data(remote)
  sha <- type_github_get_commit_sha(remote)
  asNamespace("pkgcache")$when_all(
    desc = desc, sha = sha, remote = remote, direct = direct,
    dependencies = dependencies[[2 - direct]])$
    then(type_github_make_resolution)
}

download_remote_github <- function(resolution, target, target_tree,
                                   config, cache, which, on_progress) {

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
  sha <- resolution$extra[[1]]$remotesha
  need_vignettes <- which == "resolution"

  ## 1. Check if we have a built package in the cache. We do not check the
  ## ref or the type, so the package could have been built from a local
  ## ref or from another repo. As long as the sha is the same, we are
  ## fine. If we don't require vignetted, then a package with or without
  ## vignettes is fine.

  hit <- cache$package$copy_to(
    target, package = package, sha256 = sha, built = TRUE,
    .list = c(if (need_vignettes) c(vignettes = TRUE)))
  if (nrow(hit)) {
    "!DEBUG found GH `resolution$ref`@`sha` in the cache"
    return("Had")
  }

  ## 2. Check if we have a repo snapshot in the cache.

  rel_target <- resolution$target
  subdir <- resolution$remote[[1]]$subdir
  hit <- cache$package$copy_to(
    target_tree, package = package, sha256 = sha, built = FALSE)
  if (nrow(hit)) {
    "!DEBUG found GH zip for `resolution$ref`@`sha` in the cache"
    return("Had")
  }

  ## 3. Need to download the repo

  "!DEBUG Need to download GH package `resolution$ref`@`sha`"
  urls <- resolution$sources[[1]]
  rel_zip <- paste0(rel_target, "-tree")
  type_github_download_repo(urls, target_tree, rel_zip, sha, package, cache,
                            on_progress)$
    then(function() {
      "!DEBUG Building package `resolution$package`"
      return("Got")
    })
}

type_github_download_repo <- function(urls, repo_zip, rel_zip, sha,
                                      package, cache, on_progress) {
  ## TODO: progress
  asNamespace("pkgcache")$download_file(urls, repo_zip,
                                        on_progress = on_progress)$
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
    sha1 <- tryCatch(candidate$extra[[1]]$remotesha, error = function(e) "")
    sha2 <- resolution$extra[[1]]$remotesha
    ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
    if (!ok) {
      return(structure(FALSE, reason = "Installed package sha mismatch"))
    } else {
      return(TRUE)
    }
  }

  ## 2. other refs are also good, as long as they have the same sha
  sha1 <- tryCatch(candidate$extra[[1]]$remotesha, error = function(e) "")
  sha2 <- resolution$extra[[1]]$remotesha
  ok <- is_string(sha1) && is_string(sha2) && same_sha(sha1, sha2)
  if (!ok) {
    return(structure(FALSE, reason = "Candidate package sha mismatch"))
  } else {
    return(TRUE)
  }
}

## ----------------------------------------------------------------------
## Internal functions

type_github_builtin_token <- function() {
  pats <- c(
    paste0("3687d8b", "b0556b7c3", "72ba1681d", "e5e689b", "3ec61279"),
    paste0("8ffecf5", "13a136f3d", "23bfe46c4", "2d67b3c", "966baf7b")
  )
  once_per_session(cliapp::cli_alert_warning(c(
    "Using bundled GitHub PAT. ",
    "Please add your own PAT to the env var {envvar GITHUB_PAT}"
  )))
  sample(pats, 1)
}

type_github_get_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")
  token <- Sys.getenv("GITHUB_TOKEN", NA_character_)
  if (is.na(token)) token <- Sys.getenv("GITHUB_PAT", NA_character_)
  if (is.na(token)) token <- type_github_builtin_token()
  headers <- c(headers, c("Authorization" = paste("token", token)))
  headers
}

## Returns a deferred value
#' @importFrom base64enc base64decode

type_github_get_description_data <- function(rem) {

  call <- sys.call(-1)
  user <- rem$username
  repo <- rem$repo
  ref <- if (nzchar(rem$commitish)) rem$commitish else "master"
  subdir <- if (!is.null(rem$subdir) && rem$subdir != "") {
    paste0(utils::URLencode(rem$subdir), "/")
  } else {
    ""
  }

  query <- glue(
    "query {
       repository(owner: \"<user>\", name: \"<repo>\") {
         object(expression: \"<ref>:<subdir>DESCRIPTION\") {
           ... on Blob {
             text
           }
         }
      }
    }",
    .open = "<", .close = ">"
  )
  selector <- c("data", "repository", "object", "text")

  github_query(query, selector)$
    then(function(txt) {
      if (is.null(txt)) throw(new_github_query_no_pkg_error(rem, call))
      rethrow(
        desc(text = txt),
        new_github_query_desc_parse_error(rem, call, e)
      )
    })
}

new_github_query_no_pkg_error <- function(rem, call) {
  subdir <- if (!is.null(rem$subdir) && rem$subdir != "") {
    paste0(", in directory `", rem$subdir, "`")
  } else {
    ""
  }
  msg <- glue(
    "Cannot find R package in GitHub repo ",
    "`{rem$username}/{rem$repo}`{subdir}"
  )
  structure(
    list(
      message = msg,
      call = call
    ),
    class = c("github_query_error", "error", "condition")
  )
}

new_github_query_desc_parse_error <- function(rem, call, e) {
  subdir <- if (!is.null(rem$subdir) && rem$subdir != "") {
    paste0(", in directory `", rem$subdir, "`")
  } else {
    ""
  }
  msg <- glue(
    "Cannot parse DESCRIPTION file in GitHub repo ",
    "`{rem$username}/{rem$repo}`{subdir}"
  )
  structure(
    list(
      message = msg,
      call = call
    ),
    class = c("github_query_error", "error", "condition")
  )
}

## Returns a deferred value

type_github_get_commit_sha <- function(rem) {
  user <- rem$username
  repo <- rem$repo

  if (rem$pull != "") {
    pull <- rem$pull
    query <- glue(
      "query {
         repository(owner: \"<user>\", name: \"<repo>\") {
           pullRequest(number: <pull>) {
             headRefOid
           }
         }
       }",
      .open = "<", .close = ">"
    )
    selector <- c("data", "repository", "pullRequest", "headRefOid")

  } else {
    ref <- if (nzchar(rem$commitish)) rem$commitish else "master"
    query <- glue(
      "query {
         repository(owner: \"<user>\", name: \"<repo>\") {
           object(expression: \"<ref>\") {
             oid
           }
         }
      }",
      .open = "<", .close = ">"
    )
    selector <- c("data", "repository", "object", "oid")
  }
  github_query(query, selector)
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

  meta <- c(
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteRepo = repo,
    RemoteUsername = username,
    RemotePkgRef = data$remote$ref,
    RemoteRef = if (is.null(pull)) commitish %||% "master" else NULL,
    RemotePull = pull,
    RemoteSha = sha,
    RemoteSubdir = subdir,
    GithubRepo = repo,
    GithubUsername = username,
    GithubRef = if (is.null(pull)) commitish %||% "master" else NULL,
    GitHubPull = pull,
    GithubSHA1 = sha,
    GithubSubdir = subdir)

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
    extra = list(list(remotesha = sha)),
    metadata = meta
  )
}

github_query <- function(query, selector = "data",
                         url = "https://api.github.com/graphql",
                         headers = character(), ...) {

  query; selector; url; headers; list(...)
  call <- sys.call(-1)

  headers <- c(headers, type_github_get_headers())
  data <- jsonlite::toJSON(list(query = query), auto_unbox = TRUE)
  http_post(url, data = data, headers = headers, ...)$
    catch(error = function(e) {
      throw(
        new_github_error("Cannot query GitHub, are you offline?"),
        parent = e
      )
    })$
    then(function(res) {
      json <- rawToChar(res$content %||% raw())
      obj <- if (nzchar(json)) jsonlite::fromJSON(json, simplifyVector = FALSE)

      if (res$status_code >= 300 || "errors" %in% names(obj)) {
        throw(new_github_query_error(obj, res, call))
      }

      if (length(selector)) obj[[selector]] else obj
    })
}

new_github_error <- function(...) {
  e <- new_error(...)
  class(e) <- c("github_error", class(e))
  e
}

new_github_query_error <- function(obj, response, call) {
  if (response$status_code == 401 &&
      nzchar(obj$message) && grepl("Bad credentials", obj$message)) {
    msg <- paste0(
      "Bad GitHub credentials, ",
      "make sure that your GitHub token is valid."
    )

  } else if (response$status_code >= 300) {
    return(new_http_error(response, call))

  } else {
    ghmsgs <- sub("\\.?$", ".", vcapply(obj$errors, "[[", "message"))
    types <- vcapply(obj$errors, "[[", "type")
    msg <- paste0("GitHub error: ", paste0(ghmsgs, collapse = ", "))

    if ("RATE_LIMITED" %in% types) {
      headers <- curl::parse_headers_list(response$headers)
      if ("x-ratelimit-reset" %in% names(headers)) {
        reset <- format(
          .POSIXct(headers$`x-ratelimit-reset`, tz = "UTC"),
          usetz = TRUE
        )
        msg <- paste0(msg, " Rate limit will reset at ", reset, ".")
      }
    }
  }

  structure(
    list(
      message = msg,
      call = call,
      response = response
    ),
    class = c("github_error", "error", "condition")
  )
}

new_http_error <- function(response, call) {
  asNamespace("pkgcache")$http_error(response, call)
}
