
default_remote_types <- function() {
  default <- list(
    cran = list(
      parse = parse_remote_cran,
      resolve = resolve_remote_cran,
      download = download_remote_cran,
      satisfy = satisfy_remote_cran),
    bioc = list(
      parse = parse_remote_bioc,
      resolve = resolve_remote_bioc,
      download = download_remote_bioc,
      satisfy = satisfy_remote_bioc),
    standard = list(
      parse = parse_remote_standard,
      resolve = resolve_remote_standard,
      download = download_remote_standard,
      satisfy = satisfy_remote_standard),
    github = list(
      parse = parse_remote_github,
      resolve = resolve_remote_github,
      download = download_remote_github,
      satisfy = satisfy_remote_github),
    local = list(
      parse = parse_remote_local,
      resolve = resolve_remote_local,
      download = download_remote_local,
      satisfy = satisfy_remote_local),
    deps = list(
      parse = parse_remote_deps,
      resolve = resolve_remote_deps,
      download = download_remote_deps,
      satisfy  = satisfy_remote_deps),
    installed = list(
      parse = parse_remote_installed,
      resolve = resolve_remote_installed,
      download = download_remote_installed,
      satisfy = satisfy_remote_installed),
    url = list(
      parse = parse_remote_url,
      resolve = resolve_remote_url,
      download = download_remote_url,
      satisfy = satisfy_remote_url)
  )

  modifyList(default, as.list(getOption("pkg.remote_types")))
}
