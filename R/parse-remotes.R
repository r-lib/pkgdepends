
#' Package references
#'
#' A package reference (ref) specifies a location from which an R package
#' can be obtained from. The full syntax of a reference is `type::ref`, but
#' `type` can be often omitted, the common ref types have shortcuts.
#'
#' The currently supported reference types are:
#' - `cran`: a CRAN package.
#' - `bioc`: A Bioconductor package.
#' - `standard`: a package from CRAN or Bioconductor.
#' - `github`: A package from GitHub.
#' - `local`: A local package file or directory.
#' - `installed` An installed package.
#' - `deps` The dependencies of a local package file or directory.
#'
#' If a ref does not explicitly specify a type, then the following rules
#' are applied:
#' - if the ref is a valid `standard` ref type (without the `standard::`
#'   prefix), then `standard` is used;
#' - if the ref is a valid `github` ref type (without the `github::` prefix),
#'   then `github` is used;
#' - if the ref is a GitHub URL, then `github` is used;
#' - otherwise an error is thrown.
#' 
#' @section CRAN packages:
#'
#' A package from CRAN. Full syntax:
#'
#' ```
#' [cran::]<package>[@[>=]<version> | current | last]
#' ```
#'
#' - `<package>` is a valid package name.
#' - `<version>` is either a package version, or 
#' 
#' Examples: `forecast`, `forecast@8.8`, `forecast@>=8.8`,
#' `cran::forecast`, `forecast@last`, `forecast@current`.
#'
#' Note: pkgdepends currently parses the version specification part
#' (everything after `@`), but does not use it.
#'
#' @section Bioconductor packages:
#'
#' A package from Bioconductor. The syntax is the same as for CRAN packages,
#' except of the prefix of course:
#'
#' ```
#' [bioc::]<package>[@[>=]<version> | current | last]
#' ```
#'
#' @section Standard packages:
#'
#' These are packages either from CRAN or Bioconductor, the full syntax
#' is the same as for CRAN packages, except for the prefix:
#'
#' ```
#' [standard::]<package>[@[>=]<version> | current | last]
#' ```
#'
#' @section GitHub packages:
#'
#' Packages from a GitHub repository. Full syntax:
#'
#' ```
#'[<package>=][github::]<username>/<repository>[/<subdir>][<detail>]
#' ```
#'
#' - `<package>` is the name of the package. If this is missing, then
#'   the name of the repository is used.
#' - `<username>` is a GitHub username or organization name.
#' - `<repository>` is the name of the repository.
#' - `<subdir>` optional subdirectory, if the package is within a
#'   subdirectory in the repository.
#' - `<detail>` specifies a certain version of the package, see below.
#'
#' `<detail>` may specify:
#' - a git branch, tag or (prefix of) a commit sha: `@<commitish>`;
#' - a pull request: `#<pull-request>`; or
#' - the latest release: `@*release`.
#'
#' Examples: `r-lib/crayon`, `github::r-lib/crayon`, `r-lib/crayon@84be6207`,
#' `r-lib/crayon@branch`, `r-lib/crayon#41`, `r-lib/crayon@release`.
#' 
#' For convenience GitHub HTTP URLs can also be used to specify a
#' package from GitHub. Examples:
#' - `https://github.com/r-lib/withr`
#' - A branch: `https://github.com/r-lib/withr/tree/ghactions`
#' - A tag: `https://github.com/r-lib/withr/tree/v2.1.1`
#' - A commit: `https://github.com/r-lib/withr/commit/8fbcb548e316`
#' - A pull request: `https://github.com/r-lib/withr/pull/76`
#' - A release: `https://github.com/r-lib/withr/releases/tag/v2.1.0`
#'
#' A GitHub remote string can also be used instead of an URL, for example:
#' `git@github.com:r-lib/pkgdepends.git`
#'
#' @section Local packages:
#'
#' A path that refers to a package file built with `R CMD build`, or a
#' directory that contains a package. Full syntax:
#'
#' ```
#' local::<path>
#' ````
#'
#' Examples: `local::/foo/bar/package_1.0.0.tar.gz`, `local::/foo/bar/pkg`,
#' `local::.`.
#'
#' @section Installed packages:
#'
#' This is usually used internally, but can also be used directly.
#' Full syntax:
#'
#' ```
#' installed::<path>/<package>
#' ```
#'
#' - `<path>` is the library the package is installed to.
#' - `<package>` is the package name.
#'
#' Examples: `installed::~/R/3.6/crayon`.
#'
#' @section Package dependencies:
#'
#' Usually used internally, it specifies the dependencies of a local
#' package. It can be used to download or install the dependencies of a
#' package, without downloading or installing the package itself.
#' Full syntax:
#'
#' ```
#' deps::<path>
#' ```
#'
#' Examples: `deps::/foo/bar/package_1.0.0.tar.gz`, `deps::/foo/bar/pkg`,
#' `deps::.`.
#' 
#' @name pkg_refs
#'
NULL

package_name_rx <- function() "[[:alpha:]][[:alnum:].]*[[:alnum:]]"

## CRAN and GitHub are special, because they have shorthands,
## so we need to know their regexes to find the type of the remotes

standard_rx <- function(remote_name = "standard") {
  paste0(
    "^",
    ## Optional remote type
    "(?:", remote_name, "::)?",
    ## Package name, only valid names
    "(?<package>", package_name_rx(), ")",
    ## Package version, only valid version numbers
    "(?:@(?:(?:(?<atleast>>=)?",
    "(?<version>[0-9]+[-\\.][0-9]+(?:[-\\.][0-9]+)*|current|last))))?",
    "$"
  )
}

#' Match a GH username
#'
#' * may only contain alphanumeric characters or hyphens
#' * cannot have multiple consecutive hyphens
#' * cannot begin or end with a hyphen
#' * maximum 39 characters
#'
#' Based on https://github.com/shinnn/github-username-regex
#'
#' @keywords internal

github_username_rx <- function() {
  "(?<username>(?:[a-zA-Z\\d](?:[a-zA-Z\\d-]){0,38}))"
}

github_repo_rx <- function() "(?<repo>[^/@#]+)"
github_subdir_rx <- function() "(?:/(?<subdir>(?:[^@#]*[^@#/])/?))"
github_commitish_rx <- function() "(?:@(?<commitish>[^*].*))"
github_pull_rx <- function() "(?:#(?<pull>[0-9]+))"
github_release_rx <- function() "(?:@(?<release>[*]release))"
github_detail_rx <- function() {
  sprintf(
    "(?:(?:%s)|(?:%s)|(?:%s))?",
    github_commitish_rx(),
    github_pull_rx(),
    github_release_rx()
  )
}

github_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Optional remote type
    "(?:github::)?",
    github_username_rx(), "/",
    github_repo_rx(),
    github_subdir_rx(), "?",
    ## Commit / PR / Release
    github_detail_rx(),
    "$"
  )
}

github_url_commitish_rx <- function() {
  "(?:(?:tree|commit|releases/tag)/(?<commitish>.+$))"
}

github_url_pull_rx <- function() "(?:pull/(?<pull>.+$))"

github_url_release_rx <- function() "(?:releases/)(?<release>.+$)"

github_url_detail_rx <- function() {
  glue("(?:/(?:",
       "{github_url_commitish_rx()}",
       "|{github_url_pull_rx()}",
       "|{github_url_release_rx()}",
       "))?")
}

## We need to select the shortest match here, to avoid matching a
## a .git suffix

github_url_repo_rx <- function() "(?<repo>[^/@#]+?)"

github_url_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Optional remote type
    "(?:github::)?",
    ## Optional protocol
    "(?:(?:https?://)|(?:ssh://(?:[^@]+@)?)?)",
    ## Servername
    "(?:[^/:]+)[/:]",
    ## Username
    github_username_rx(), "/",
    ## Repo
    github_url_repo_rx(),
    ## subdir, always empty
    "(?<subdir>)",
    ## Optional Extension
    "(?:[.]git)?",
    ## Commit / PR / Release
    github_url_detail_rx(),
    "$"
  )
}

remote_type_rx <- function() {
  paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Remote type
    "(?:(?<type>[-_[:alnum:]]+)::)?",
    ## Rest of ref
    "(?<rest>.*)$"
  )
}

#' @importFrom rematch2 re_match

type_default_parse <- function(refs, ...) {
  m <- re_match(refs, remote_type_rx())
  lapply_rows(m, function(x)
    list(package = x$package, type = x$type, rest = x$rest, ref = x$.text)
  )
}

get_remote_types <- function(refs) {
  m <- re_match(refs, remote_type_rx())
  types <- m$type

  types[types == "" & grepl(standard_rx(), refs, perl = TRUE)] <- "standard"
  types[types == "" & grepl(github_rx(), refs, perl = TRUE)] <- "github"
  types[types == "" & grepl(github_url_rx(), refs, perl = TRUE)] <- "github"

  if (any(bad <- types == "")) {
    stop("Can't parse remotes: ", paste(refs[bad], collapse = ", "))
  }

  types
}

#' Parse package location references
#'
#' See [pkg_refs] for more about supported package references.
#'
#' @param refs Character vector of references.
#' @param remote_types Custom remote types can be added here, this is
#'   for advanced use, and experimental currently.
#' @param ... Additional arguments are passed to the individual parser
#'   functions.
#' @return `parse_pkg_refs()` retuns a list of parsed references.
#' `parse_pkg_ref()` returns one parsed reference. A parsed reference is
#' a list, with at least elements:
#' - `ref`: The original reference string.
#' - `type`: The reference type.
#' - `package`: The package name.
#' It typically contains additional data, specific to the various
#' reference types. See [pkg_refs] for details.
#' The parsed reference always has class `remote_ref_<type>` and
#' `remote_ref`.
#'
#' @export

parse_pkg_refs <- function(refs, remote_types = NULL, ...) {
  remote_types <- c(default_remote_types(), remote_types)
  types <- get_remote_types(refs)
  unique_types <- unique(types)
  res <- vector("list", length(refs))

  if (any(bad <- setdiff(unique_types, names(remote_types)))) {
    stop("Unknown remote type(s): ", format_items(bad))
  }

  for (this in unique_types) {
    parser <- remote_types[[this]]$parse %||% type_default_parse
    this_refs <- refs[types == this]
    new_remotes <- parser(this_refs, ...)
    new_remotes <- lapply(new_remotes, function(x) { x$type <- this; x })
    new_remotes <- lapply(
      new_remotes,
      add_class,
      c(paste0("remote_ref_", this), "remote_ref")
    )
    res[types == this] <- new_remotes
  }
  res
}

#' @param ref A package reference, like `refs`, but a length one vector,
#' for convenience.
#' @export
#' @rdname parse_pkg_refs

parse_pkg_ref <- function(ref, remote_types = NULL, ...) {
  assert_that(is_string(ref))
  parse_pkg_refs(ref, remote_types = remote_types, ...)[[1]]
}
