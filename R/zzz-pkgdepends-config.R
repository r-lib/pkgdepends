
windows_archs <- function() c("prefer-x64", "both")

default_windows_archs <- function() {
  if (getRversion() < "4.2.0") "both" else "prefer-x64"
}

default_update_after <- function() as.difftime(24, units = "hours")

env_decode_dependencies <- function(x, name) {
  if (tolower(x) %in% c("yes", "true", "1", "on")) return(TRUE)
  if (tolower(x) %in% c("no", "false", "0", "off")) return(FALSE)
  if (tolower(x) == "na") return(NA)
  strsplit(x, ";", fixed = TRUE)[[1]]
}

env_decode_difftime <- function(x, name) {
  if (nchar(x) >= 2) {
    unit <- substr(x, nchar(x), nchar(x))
    unit <- c(s = "secs", m = "mins", h = "hours", d = "days")[unit]
    qty <- suppressWarnings(as.numeric(substr(x, 1, nchar(x) - 1)))
    if (!is.na(unit) && !is.na(qty)) {
      return(as.difftime(qty, units = unit))
    }
  }
  throw(pkg_error(
    "Invalid time interval specification in {.envvar {name}} environment
     variable: {.str {x}}",
    i = "It must have the form {.code <number><unit>}.",
    i = "The unit must be a single letter: {.code s} (seconds),
     {.code m} (minutes), {.code h} (hours) or {.code d} (days).",
    i = "Examples: {.emph 60s}, {.emph 2h}, {.emph 1d}."
  ))
}

default_sysreqs <- function() {
  if (Sys.getenv("CI") != "true") return(FALSE)
  if (Sys.info()[["sysname"]] != "Linux") return(FALSE)
  dist <- detect_linux()
  sysreqs2_is_supported(dist$distribution, dist$release)
}

default_sysreqs_sudo <- function() {
  if (os_type() != "unix") {
    FALSE
  } else {
    euid <- get_euid()
    is.na(euid) || euid != 0
  }
}

default_sysreqs_verbose <- function() {
  Sys.getenv("CI") != ""
}

default_sysreqs_rspm_url <- function() {
  Sys.getenv("RSPM_ROOT", "https://packagemanager.posit.co")
}

default_sysreqs_rspm_repo_id <- function() {
  Sys.getenv("RSPM_REPO_ID", "1")
}

pkgdepends_config <- sort_by_name(list(
  # -----------------------------------------------------------------------
  library = list(
    type = "string_or_null",
    docs =
      "Package library to install packages to. It is also used for
       already installed packages when considering dependencies in
       [dependency lookup][pkg_deps] or
       [package installation][pkg_installation_proposal]. Defaults to the
       first path in [.libPaths()].",
    docs_pak =
      "Package library to install packages to. It is also used for
       already installed packages when considering dependencies."
  ),

  # -----------------------------------------------------------------------
  cache_dir = list(
    type = "string",
    default = detect_download_cache_dir,
    docs =
      "Directory to download the packages to. Defaults to a temporary
       directory within the R session temporary directory, see
       [base::tempdir()]."
  ),

  # -----------------------------------------------------------------------
  package_cache_dir = list(
    type = "string",
    docs =
      "Package cache location of [`pkgcache::package_cache`]. The default
       is the pkgcache default.",
    docs_pak =
      "Location of the package cache on the disk. See
       [pak::cache_summary()]. Default is selected by pkgcache."
  ),

  # -----------------------------------------------------------------------
  metadata_cache_dir = list(
    type = "string",
    default = function() tempfile(),
    # TODO: do not link to pkgcache docs from pak
    docs =
      "Location of metadata replica of
       [`pkgcache::cranlike_metadata_cache`]. Defaults to a temporary
       directory within the R session temporary directory, see
       [base::tempdir()]."
  ),

  # -----------------------------------------------------------------------
  platforms = list(
    type = "character",
    default = default_platforms,
    # TODO: do not link to pkgdepends docs from pak
    docs =
      "Character vector of platforms to _download_ or _install_ packages
       for. See [pkgdepends::default_platforms()] for possible platform
       names. Defaults to the platform of the current R session, plus
       `\"source\"`."
  ),

  # -----------------------------------------------------------------------
  windows_archs = list(
    type = "string",
    default = default_windows_archs,
    docs =
  # we can't indent this "correctly" because markdown will take it as code
  "Character scalar specifying which architectures
   to download/install for on Windows. Its possible values are:

   - `\"prefer-x64\"`: Generally prefer x64 binaries. If the current R
     session is `x64`, then we download/install x64 packages.
     (These packages might still be multi-architecture binaries!)
     If the current R session is `i386`, then we download/install
     packages for both architectures. This might mean compiling
     packages from source if the binary packages are for `x64` only,
     like the CRAN Windows binaries for R 4.2.x currently.
     `\"prefer-x64\"` is the default for R 4.2.0 and later.
   - `\"both\"`: Always download/install packages for both `i386` and
     `x64` architectures. This might need compilation from source
     if the available binaries are for `x64` only, like the CRAN
     Windows binaries for R 4.2.x currently. `\"both\"` is the default
     for R 4.2.0 and earlier."
  ),

  # -----------------------------------------------------------------------
  cran_mirror = list(
    type = "string",
    default = default_cran_mirror,
    docs =
      "CRAN mirror to use. Defaults to the `repos` option
       (see [base::options()]), if that's not set then
       `https://cran.rstudio.com`.",
    docs_pak =
      "CRAN mirror to use. Defaults to the `repos` option
       (see [base::options()]), if that's not set then
       `https://cran.rstudio.com`. See also [pak::repo_add()] and
       [pak::repo_get()]"
  ),

  # -----------------------------------------------------------------------
  dependencies = list(
    type = "dependencies",
    default = pkg_dep_types_hard,
    # pak functions take this as an argument, so we do not need it, for now
    pak = FALSE,
    docs =
      "Dependencies to consider or download or install.
       Defaults to the hard dependencies, see
       [pkgdepends::pkg_dep_types_hard()]. The following values are
       supported in the `PKG_DEPENDENCIES` environment variable:
       `\"TRUE\"`, `\"FALSE\"`, `\"NA\"`, or a semicolon separated list of
       dependency types. See [pkgdepends::as_pkg_dependencies()] for
       details."
  ),

  # -----------------------------------------------------------------------
  r_versions = list(
    type = "character",
    default = current_r_version,
    check = is_r_version_list,
    docs =
      "Character vector, R versions to download or install
       packages for. It defaults to the current R version."
  ),

  # -----------------------------------------------------------------------
  build_vignettes = list(
    type = "flag",
    default = FALSE,
    docs =
      "Whether to build vignettes for package trees.
       This is only used if the package is obtained from a package tree,
       and not from a source (or binary) package archive. By default
       vignettes are not built in this case. If you set this to `TRUE`,
       then you need to make sure that the vignette builder packages are
       available, as these are not installed by default currently."
  ),

  # -----------------------------------------------------------------------
  metadata_update_after = list(
    type = "difftime",
    default = default_update_after,
    docs =
      "A time interval as a [difftime] object. pkgdepends will update the
       metadata cache if it is older than this. The default is one day.
       The `PKG_METADATA_UPDATE_AFTER` environment variable may be set
       in seconds (`s` suffix), minutes (`m` suffix), hours (`h` suffix),
       or days (`d` suffix). E.g: `1d` means one day.",
    docs_pak =
      "A time interval as a [difftime] object. pak will update the
       metadata cache if it is older than this. The default is one day.
       The `PKG_METADATA_UPDATE_AFTER` environment variable may be set
       in seconds (`s` suffix), minutes (`m` suffix), hours (`h` suffix),
       or days (`d` suffix). E.g: `1d` means one day."
  ),

  # -----------------------------------------------------------------------
  sysreqs = list(
    type = "flag",
    default = default_sysreqs,
    docs =
      "Whether to look up and install system requirements.
       By default this is `TRUE` if the `CI` environment variable is set
       and the operating system is a supported Linux distribution:
       CentOS, Debian, Fedora, openSUSE, RedHat Linux, Ubuntu Linux or SUSE
       Linux Enterprise. The default will change as new platforms gain
       system requirements support."
  ),

  sysreqs_dry_run = list(
    type = "flag",
    default = FALSE,
    docs =
      "If `TRUE`, then pkgdepends only prints the system commands to
       install system requirements, but does not execute them.",
    docs_pak =
      "If `TRUE`, then pak only prints the system commands to
       install system requirements, but does not execute them."
  ),

  # -----------------------------------------------------------------------
  sysreqs_rspm_repo_id = list(
    type = "string",
    default = default_sysreqs_rspm_repo_id,
    docs =
      "Posit Package Manager (formerly RStudio Package Manager) repository
       id to use for CRAN system requirements lookup. Defaults to the
       `RSPM_REPO_ID` environment variable, if set. If not set, then it
        defaults to `1`."
  ),

  # -----------------------------------------------------------------------
  sysreqs_rspm_url = list(
    type = "string",
    default = default_sysreqs_rspm_url,
    docs = "Root URL of Posit Package Manager (formerly RStudio Package
       Manager) for system requirements lookup. By default the `RSPM_ROOT`
       environment variable is used, if set. If not set,
       it defaults to `https://packagemanager.posit.co`."
  ),

  # -----------------------------------------------------------------------
  sysreqs_sudo = list(
    type = "flag",
    default = default_sysreqs_sudo,
    docs =
      "Whether to use `sudo` to install system requirements,
       on Unix. By default it is `TRUE` on Linux if the effective user id
       of the current process is not the `root` user."
  ),

  # -----------------------------------------------------------------------
  sysreqs_verbose = list(
    type = "flag",
    default = default_sysreqs_verbose,
    docs =
      "Whether to echo the output of system requirements installation.
       Defaults to `TRUE` if the `CI` environment variable is set."
  ),

  # -----------------------------------------------------------------------
  use_bioconductor = list(
    type = "flag",
    default = TRUE,
    docs =
      "Whether to automatically use the Bioconductor repositories.
       Defaults to `TRUE`."
  ),

  # -----------------------------------------------------------------------
  # Internal
  goal = list(
    type = "string",
    default = "unknown"
  )
))

#' pkgdepends configuration
#' @name pkg_config
#' @aliases pkgdepends-config
#'
#' @description
#' Configuration entries for several pkgdepends classes.
#'
#' @export
#' @details
#' pkgdepends configuration is set from several source. They are, in the
#' order of preference:
#' * Function arguments, e.g. the `config` argument of
#'   [new_pkg_installation_proposal()].
#' * Global options, set via [options()]. The name of the global option
#'   is the `pkg.` prefix plus the name of the pkgdepends configuration
#'   entry. E.g. `pkg.platforms`.
#' * Environment variables. The name of the environment variable is the
#'   `PKG_` prefix, plus the name of the pkgdepends configuration entry, in
#'   uppercase. E.g. `PKG_PLATFORMS`.
#' * Default values.
#'
#' Not all classes use all entries. E.g. a [`pkg_download_proposal`] is not
#' concerned about package libraries, so it'll ignore the `library`
#' configuration entry.
#'
#' Call `current_config()` to print the current configuration.
#'
#' # Configuration entries
#'
#' `r generate_config_docs()`

current_config <- function() {
  conf <- config$new("pkg")
  conf$add_type("dependencies", is_dependencies, env_decode_dependencies)
  conf$add_type("difftime", is_difftime, env_decode_difftime)

  map_named(pkgdepends_config, function(name, entry) {
    type <- entry$type %||% "character"
    conf$add(
      name,
      type,
      default = entry$default,
      check = entry$check %||% type,
      env_decode = entry$env_decode %||% type
    )
  })

  conf$lock()
  conf
}
