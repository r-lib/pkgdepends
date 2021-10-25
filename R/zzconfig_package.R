
windows_archs <- function() c("prefer-x64", "both")

default_windows_archs <- function() {
  if (getRversion() < "4.2.0") "both" else "prefer-x64"
}

default_update_after <- function() as.difftime(24, units = "hours")

#' pkgdepends configuration
#' @name pkgdepends-config
#'
#' @description
#' Configuration entries for several pkgdepends classes. Not all classes
#' use all entries. E.g. a [`pkg_download_proposal`] is not concerned about
#' package libraries, so it'll ignore the `library` configuration entry.
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
#'   uppercase. E.g. `PKG_PLATFORMS`. Some configuration entries cannot be
#'   set via environment variables, as documented below.
#' * Default values.
#'
#' Call `current_config()` to print the current configuration.
#'
#' # Configuration entries

current_config <- function() {
  conf <- config$new("pkg")

  #' * `library`: package library to use for checking already installed
  #'   packages when considering dependencies in
  #'   [dependency lookup][pkg_deps] or
  #'   [package installation][pkg_installation_proposal]. Defaults to the
  #'   first path in [.libPaths()].
  conf$add("library", "string_or_null")

  #' * `cache_dir`: directory to download the packages to. Defaults to a
  #'   temporary directory within the R session temporary directory, see
  #'   [base::tempdir()].
  conf$add("cache_dir", "string", detect_download_cache_dir)

  #' * `package_cache_dir`: package cache location of
  #'   [`pkgcache::package_cache`]. The default is the pkgcache default.
  conf$add("package_cache_dir")

  #' * `metadata_cache_dir`: location of metadata replica of
  #'   [`pkgcache::cranlike_metadata_cache`]. Defaults to a temporary
  #'   directory within the R session temporary directory, see
  #'   [base::tempdir()].
  conf$add("metadata_cache_dir", "string", tempfile)

  #' * `platforms`: Character vector of platforms to _download_ or _install_
  #'   for. See [default_platforms()] for possible platform names.
  conf$add("platforms", "character", default_platforms)

  #' * `windows_archs`: Character scalar specifying which architectures
  #'   to download/install for on Windows. Its possible values are:
  #'   - `"prefer-x64"`: Generally prefer x64 binaries. If the current R
  #'     session is `x64`, then we download/install x64 packages.
  #'     (These packages might still be multi-architecture binaries!)
  #'     If the current R session is `i386`, then we download/install
  #'     packages for both architectures. This might mean compiling
  #'     packages from source if the binary packages are for `x64` only,
  #'     like the CRAN Windows binaries for R 4.2.x currently.
  #'     `"prefer"` is the default from R 4.2.0.
  #'   - `"both"`: Always download/install packages for both `i386` and
  #'     `x64` architectures. This might need compilation from source
  #'     if the available binaries are for `x64` only, like the CRAN
  #'     Windows binaries for R 4.2.x currently. `"both"` is the default
  #'     before R 4.2.0.
  conf$add("windows_archs", "string", default_windows_archs)

  #' * `cran_mirror`: CRAN mirror to use. Defaults to the `repos` option
  #'   (see [base::options()]), if that's not set then
  #'   `https://cran.rstudio.com`.
  conf$add("cran_mirror", "string", default_cran_mirror)

  #' * `dependencies`: Dependencies to consider or download or install.
  #'   Defaults to the hard dependencies, see [pkg_dep_types_hard()].
  #'   You cannot set this configuration entry via an environment variable
  #'   currently.
  conf$add("dependencies", "custom", pkg_dep_types_hard, is_dependencies)

  #' * `r_versions`: Character vector, R versions to download or install
  #'   packages for. It defaults to the current R version.
  conf$add("r_versions", "character", current_r_version, is_r_version_list)

  #' * `build_vignettes`: Whether to build vignettes for package trees.
  #'   This is only used if the package is obtained from a package tree,
  #'   and not from a source (or binary) package archive. By default
  #'   vignettes are not built in this case. If you set this to `TRUE`,
  #'   then you need to make sure that the vignette builder packages are
  #'   available, as these are not installed by default currently.
  conf$add("build_vignettes", "flag", FALSE)

  #' * `metadata_update_after`: A time interval as a [difftime] object.
  #'   pkgdepends will update the metadata cache if it is older than this.
  #'   The default is one day.
  #'   You cannot set this configuration entry via an environment variable
  #'   currently.
  conf$add("metadata_update_after", "custom", default_update_after, is_difftime)

  conf
}
