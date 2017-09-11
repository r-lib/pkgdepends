
known_remotes <- function() {
  names(remote_regexps())
}

remote_regexps <- function() {

  package_name_rx <- "[[:alpha:]][[:alnum:].]*[[:alnum:]]"

  github_commitish_rx <- "(?:@(?<commitish>[^*].*))"
  github_pull_rx <- "(?:#(?<pull>[0-9]+))"
  github_release_rx <- "(?:@(?<release>[*]release))"
  github_detail <- sprintf(
    "(?:(?:%s)|(?:%s)|(?:%s))?",
    github_commitish_rx,
    github_pull_rx,
    github_release_rx
  )

  list(
    cran = paste0(
      "^",
      ## Optional remote type
      "(?:cran::)?",
      ## Package name, only valid names
      "(?<package>", package_name_rx, ")",
      ## Package version, only valid version numbers
      "(?:@(?:(?:(?<atleast>>=)?",
      "(?<version>[0-9]+[-\\.][0-9]+(?:[-\\.][0-9]+)*|current|last))))?",
      "$"
    ),
    github = paste0(
      "^",
      ## Optional package name
      "(?:(?<package>", package_name_rx, ")=)?",
      ## Optional remote type
      "(?:github::)?",
      ## Username
      "(?<username>(?:[^/]+))/",
      ## Repo
      "(?<repo>[^/@#]+)",
      ## Subdirectory
      "(?:/(?<subdir>(?:[^@#]*[^@#/])/?))?",
      ## Commit / PR / Release
      github_detail,
      "$"
    )
  )
}

get_remote_types <- function(specs) {
  rx <- remote_regexps()
  res <- rep(NA_character_, length(specs))

  try <- 1
  while (any(is.na(res)) && try <= length(rx)) {
    res[ grepl(rx[[try]], specs, perl = TRUE) ] <- names(rx)[try]
    try <- try + 1
  }

  res
}

parse_postprocess <- list()
parse_postprocess$github <- function(spec) {
  if (! nzchar(spec$package)) spec$package <- spec$repo
  spec
}

parse_remotes <- function(specs) {
  types <- get_remote_types(specs)
  rx <- remote_regexps()
  unique_types <- unique(types)
  res <- list()
  for (this in unique_types) {
    this_specs <- specs[types == this]
    parsed_specs <- re_match(this_specs, rx[[this]])
    parsed_specs$ref <- parsed_specs$.text
    cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
    parsed_specs <- parsed_specs[, cn]
    parsed_specs$type <- this
    new_remotes <- lapply(
      seq_len(nrow(parsed_specs)),
      function(i) as.list(parsed_specs[i,])
    )
    new_remotes <- lapply(new_remotes, parse_postprocess[[this]] %||% identity)
    res <- c(res, new_remotes)
  }
  res
}
