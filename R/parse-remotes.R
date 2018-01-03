
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

github_commitish_rx <- function() "(?:@(?<commitish>[^*].*))"
github_pull_rx <- function() "(?:#(?<pull>[0-9]+))"
github_release_rx <- function() "(?:@(?<release>[*]release))"
github_detail <- function() {
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
    ## Username
    "(?<username>(?:[[:alnum:]-]+))/",
    ## Repo
    "(?<repo>[^/@#]+)",
    ## Subdirectory
    "(?:/(?<subdir>(?:[^@#]*[^@#/])/?))?",
    ## Commit / PR / Release
    github_detail(),
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
    "(?<username>[^/]+)/",
    ## Repo
    "(?<repo>[^/#@.]+)",
    ## subdir, always empty
    "(?<subdir>)",
    ## Optional Extension
    "(?:[.]git)?",
    ## Commit / PR / Release
    github_url_detail_rx(),
    "$"
  )
}

#' @importFrom rematch2 re_match

get_remote_types <- function(specs) {
  remote_type_rx <- paste0(
    "^",
    ## Optional package name
    "(?:(?<package>", package_name_rx(), ")=)?",
    ## Remote type
    "(?:(?<type>[-_[:alpha:]]+)::)?"
    ## Rest of ref
  )

  m <- re_match(specs, remote_type_rx)
  types <- m$type

  types[types == "" & grepl(standard_rx(), specs, perl = TRUE)] <- "standard"
  types[types == "" & grepl(github_rx(), specs, perl = TRUE)] <- "github"
  types[types == "" & grepl(github_url_rx(), specs, perl = TRUE)] <- "github"

  if (any(bad <- types == "")) {
    stop("Can't parse remotes: ", paste(specs[bad], collapse = ", "))
  }

  types
}

parse_remotes <- function(specs) {
  types <- get_remote_types(specs)
  unique_types <- unique(types)
  res <- vector("list", length(specs))

  for (this in unique_types) {
    this_specs <- specs[types == this]
    class(this_specs) <- c(paste0("remote_specs_", this), "remote_specs")
    new_remotes <- parse_remote(this_specs)
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
