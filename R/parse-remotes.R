
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

github_rx <- function() {

  github_commitish_rx <- "(?:@(?<commitish>[^*].*))"
  github_pull_rx <- "(?:#(?<pull>[0-9]+))"
  github_release_rx <- "(?:@(?<release>[*]release))"
  github_detail <- sprintf(
    "(?:(?:%s)|(?:%s)|(?:%s))?",
    github_commitish_rx,
    github_pull_rx,
    github_release_rx
  )

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
    github_detail,
    "$"
  )
}

github_url_rx <- function() {
  commitish_rx <- "(?:(?:tree|commit|releases/tag)/(?<commitish>.+$))"
  pull_rx <- "(?:pull/(?<pull>.+$))"
  release_rx <- "(?:releases/)(?<release>.+$)"
  detail_rx <- glue("(?:/(?:{commitish_rx}|{pull_rx}|{release_rx}))?")

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
    ## Optional Extension
    "(?:[.]git)?",
    ## Commit / PR / Release
    detail_rx,
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
