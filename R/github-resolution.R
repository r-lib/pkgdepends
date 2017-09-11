
get_github_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")

  if (nzchar(token <- Sys.getenv("GITHUB_TOKEN")) ||
      nzchar(token <- Sys.getenv("GITHUB_PAT"))) {
    headers <- c(headers, c("Authorization" = paste("token", token)))
  }
  headers
}

get_github_description_url <- function(rem) {
  glue(
    "https://raw.githubusercontent.com/{rem$username}",
    "/{rem$repo}/{commitish}/{rem$subdir}/DESCRIPTION",
    commitish = if (nzchar(rem$commitish)) rem$commitish else "master"
  )
}

get_github_commit_url <- function(rem) {
  glue(
    "https://api.github.com/repos/{rem$username}/{rem$repo}",
    "/git/refs/heads/{commitish}",
    commitish = if (nzchar(rem$commitish)) rem$commitish else "master"
  )
}

## Returns a deferred value

get_github_description_data <- function(rem) {
  description_url <- get_github_description_url(rem)
  http_get(description_url, headers = get_github_headers())$
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

get_github_commit_sha <- function(rem) {
  commit_url <- get_github_commit_url(rem)
  http_get(commit_url, headers = get_github_headers())$
    then(function(resp) {
      cdata <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      cdata$object$sha
    })
}

remotes__resolve_ref_github_deps <- function(self, private, rem, deps,
                                             remotes, dependencies) {

  remotes <- na.omit(remotes)
  remotes_deps_packages <- vcapply(parse_remotes(remotes), "[[", "package")
  regular_deps <- setdiff(
    clean_package_deps(deps, dependencies),
    remotes_deps_packages
  )
  c(remotes, regular_deps)
}

#' @importFrom jsonlite fromJSON
#' @importFrom desc desc

remotes__resolve_ref_github <- function(self, private, rem) {

  message("Scheduling resolution of ", rem$ref)
  dependencies <- private$cfg$get("config:dependencies")

  ## Get the DESCRIPTION data, and the SHA we need
  desc <- get_github_description_data(rem)
  sha <- get_github_commit_sha(rem)
  done_data <- when_all(desc = desc, sha = sha)$
    then(function(data) {
      deps <- private$resolve_ref_github_deps(
        rem, data$desc$deps, data$desc$remotes, dependencies)
      dep_rems <- parse_remotes(deps)
      lapply(dep_rems, function(r) private$resolve_ref(r))
      files <- list(
        source = glue(
          "https://api.github.com/repos/{rem$username}/{rem$repo}",
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
      list(files = list(files), remote = rem, status = "OK")
    })
}
