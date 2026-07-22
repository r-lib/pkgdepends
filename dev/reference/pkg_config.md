# pkgdepends configuration

Configuration entries for several pkgdepends classes.

## Usage

``` r
current_config()
```

## Details

pkgdepends configuration is set from several source. They are, in the
order of preference:

- Function arguments, e.g. the `config` argument of
  [`new_pkg_installation_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md).

- Global options, set via
  [`options()`](https://rdrr.io/r/base/options.html). The name of the
  global option is the `pkg.` prefix plus the name of the pkgdepends
  configuration entry. E.g. `pkg.platforms`.

- Environment variables. The name of the environment variable is the
  `PKG_` prefix, plus the name of the pkgdepends configuration entry, in
  uppercase. E.g. `PKG_PLATFORMS`.

- Default values.

Not all classes use all entries. E.g. a
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
is not concerned about package libraries, so it'll ignore the `library`
configuration entry.

Call `current_config()` to print the current configuration.

## Configuration entries

- `build_vignettes`: Whether to build vignettes for package trees. This
  is only used if the package is obtained from a package tree, and not
  from a source (or binary) package archive. By default vignettes are
  not built in this case. If you set this to `TRUE`, then you need to
  make sure that the vignette builder packages are available, as these
  are not installed by default currently.

- `cache_dir`: Directory to download the packages to. Defaults to a
  temporary directory within the R session temporary directory, see
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- `configure_args`: Extra `--configure-args` to pass to `R CMD INSTALL`
  when building packages from source. It can be a single string, which
  is then used for all packages built from source. It can also be a
  named character vector (or a named list of character vectors) to set
  configure arguments for individual packages: the names are package
  names and the values are the corresponding configure arguments.
  Defaults to the `configure.args` option (see
  [`base::options()`](https://rdrr.io/r/base/options.html)), for
  compatibility with
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).
  The `PKG_CONFIGURE_ARGS` environment variable may be either a single
  string (used for all packages), or a semicolon separated list of
  `<package>=<args>` entries to set arguments for individual packages.

- `configure_vars`: Extra `--configure-vars` to pass to `R CMD INSTALL`
  when building packages from source. It can be a single string, which
  is then used for all packages built from source. It can also be a
  named character vector (or a named list of character vectors) to set
  configure variables for individual packages: the names are package
  names and the values are the corresponding configure variables.
  Defaults to the `configure.vars` option (see
  [`base::options()`](https://rdrr.io/r/base/options.html)), for
  compatibility with
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).
  The `PKG_CONFIGURE_VARS` environment variable may be either a single
  string (used for all packages), or a semicolon separated list of
  `<package>=<vars>` entries to set variables for individual packages.

- `cran_mirror`: CRAN mirror to use. Defaults to the `repos` option (see
  [`base::options()`](https://rdrr.io/r/base/options.html)), if that's
  not set then `https://cran.rstudio.com`.

- `dependencies`: Dependencies to consider or download or install.
  Defaults to the hard dependencies, see
  [`pkg_dep_types_hard()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_dep_types.md).
  The following values are supported in the `PKG_DEPENDENCIES`
  environment variable: `"TRUE"`, `"FALSE"`, `"NA"`, or a semicolon
  separated list of dependency types. See
  [`as_pkg_dependencies()`](https://r-lib.github.io/pkgdepends/dev/reference/as_pkg_dependencies.md)
  for details.

- `git_submodules`: Whether or not to update submodules in git
  repositories. This affects `git::` and `gitlab::` package sources
  only. If the R package is in a subdirectory then only the submodules
  within that directory are updated. If a submodule appears in
  `.Rbuildignore`, then it is skipped.

- `ignore_dev_library`: Whether to ignore library directories called
  `__dev_lib__`.

- `include_linkingto`: Whether to always include `LinkingTo`
  dependencies in the solution of and installation, even if they are not
  needed because the packages are installed from binaries. This is
  sometimes useful, see e.g. <https://github.com/r-lib/pak/issues/485>
  for an example use case. It is also needed if a repository (Posit
  Package Manager typically) might serve a source package instead of a
  binary, so that the source package can be built, see
  <https://github.com/r-lib/pak/issues/891>.

- `library`: Package library to install packages to. It is also used for
  already installed packages when considering dependencies in
  [dependency
  lookup](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md)
  or [package
  installation](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md).
  Defaults to the first path in
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

- `metadata_cache_dir`: Location of metadata replica of
  [`pkgcache::cranlike_metadata_cache`](https://r-lib.github.io/pkgcache/reference/cranlike_metadata_cache.html).
  Defaults to a temporary directory within the R session temporary
  directory, see
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- `metadata_update_after`: A time interval as a
  [difftime](https://rdrr.io/r/base/difftime.html) object. pkgdepends
  will update the metadata cache if it is older than this. The default
  is one day. The `PKG_METADATA_UPDATE_AFTER` environment variable may
  be set in seconds (`s` suffix), minutes (`m` suffix), hours (`h`
  suffix), or days (`d` suffix). E.g: `1d` means one day.

- `package_cache_dir`: Package cache location of
  [`pkgcache::package_cache`](https://r-lib.github.io/pkgcache/reference/package_cache.html).
  The default is the pkgcache default.

- `platforms`: Character vector of platforms to *download* or *install*
  packages for. See
  [`default_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/default_platforms.md)
  for possible platform names. Defaults to the platform of the current R
  session, plus `"source"`.

- `r_versions`: Character vector, R versions to download or install
  packages for. It defaults to the current R version.

- `sysreqs`: Whether to automatically look up and install system
  requirements. If `TRUE`, then `r pak_or_pkgdepends()` will try to
  install required system packages. If `FALSE`, then system requirements
  are still printed (including OS packages on supported platforms), but
  they are not installed. By default it is `TRUE` on supported
  platforms, if the current user is the root user or password-less
  `sudo` is configured for the current user.

- `sysreqs_db_update`: Whether to try to update the system requirements
  database from GitHub. If the update fails, then the cached or the
  build-in database if used. Defaults to TRUE.

- `sysreqs_db_update_timeout`: Timeout for the system requirements
  database update. Defaults to five seconds, except if the `CI`
  environment variable is set, then it is one minute.

- `sysreqs_dry_run`: If `TRUE`, then pkgdepends only prints the system
  commands to install system requirements, but does not execute them.

- `sysreqs_platform`: The platform to use for system requirements
  lookup. On Linux, where system requirements are currently supported,
  it must be a string containing the distribution name and release,
  separated by a dash. E.g.: `"ubuntu-22.04"`, or `"rhel-9"`.

- `sysreqs_rspm_repo_id`: Posit Package Manager (formerly RStudio
  Package Manager) repository id to use for CRAN system requirements
  lookup. Defaults to the `RSPM_REPO_ID` environment variable, if set.
  If not set, then it defaults to `1`.

- `sysreqs_rspm_url`: Root URL of Posit Package Manager (formerly
  RStudio Package Manager) for system requirements lookup. By default
  the `RSPM_ROOT` environment variable is used, if set. If not set, it
  defaults to `https://packagemanager.posit.co`.

- `sysreqs_sudo`: Whether to use `sudo` to install system requirements,
  on Unix. By default it is `TRUE` on Linux if the effective user id of
  the current process is not the `root` user.

- `sysreqs_update`: Whether to try to update system packages that are
  already installed. It defaults to `TRUE` on CI systems: if the `CI`
  environment variable is set to `true`.

- `sysreqs_verbose`: Whether to echo the output of system requirements
  installation. Defaults to `TRUE` if the `CI` environment variable is
  set.

- `use_bioconductor`: Whether to automatically use the Bioconductor
  repositories. Defaults to `TRUE`.

- `windows_archs`: Character scalar specifying which architectures to
  download/install for on Windows. Its possible values are:

  - `"prefer-x64"`: Generally prefer x64 binaries. If the current R
    session is `x64`, then we download/install x64 packages. (These
    packages might still be multi-architecture binaries!) If the current
    R session is `i386`, then we download/install packages for both
    architectures. This might mean compiling packages from source if the
    binary packages are for `x64` only, like the CRAN Windows binaries
    for R 4.2.x currently. `"prefer-x64"` is the default for R 4.2.0 and
    later.

  - `"both"`: Always download/install packages for both `i386` and `x64`
    architectures. This might need compilation from source if the
    available binaries are for `x64` only, like the CRAN Windows
    binaries for R 4.2.x currently. `"both"` is the default for R 4.2.0
    and earlier.

## Forwarded configuration

The following entries are not used directly, they are handled by other
packages that perform the corresponding requests. They are set via R
options and environment variables, but, unlike the entries above, they
cannot be set via the `config` argument, and their option and
environment variable names may differ from the `pkg.` and `PKG_` naming
convention.

- `pkg_http_retry` option, `PKG_HTTP_RETRY` environment variable:
  Whether to retry failed HTTP requests. It can be `TRUE` to retry with
  the default settings, or `FALSE` to never retry. Defaults to `TRUE`.
  This entry is handled by pkgcache, which performs all HTTP requests.
