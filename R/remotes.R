
#' Class for package dependency resolution and package downloads
#'
#' @section Usage:
#' ```
#' r <- remotes$new(specs, config = list())
#'
#' r$resolve()
#' r$async_resolve()
#' r$get_resolution()
#'
#' r$download()
#' r$async_download()
#' r$get_download_status()
#'
#' @section Arguments:
#' * `specs`: Package specifications. See 'Remote Types' below.
#' * `config`: Custom configuration, a named list. See the list of options
#'   below.
#'
#' @section Details:
#'
#' `$new()` creates a new resolution/download task. The packages must be
#' supplied.
#'
#' `$resolve()` resolves all remote packages, and their dependencies.
#' The result of the resolution is stored internally in the `remotes`
#' object, and also returned. It can be queried later with
#' `$get_resolution()`. See `Resolution Table` below.
#'
#' `$async_resolve()` returns a deferred value for the resolution of
#' all remote packages. The deferred value resolves to the same value as
#' returned by `$resolve()`.
#'
#' `$get_resolution()` returns the result of the resolution. See
#' `Resolution Table` below.
#'
#' @section Remote types:
#' The following remote types are currently supported:
#'
#' * CRAN packages: 
#'     \preformatted{
#'     [cran::]<package>[@[>=]<version> | @current | @last]
#'     }
#' * Packages from GitHub repositories
#'     \preformatted{
#'     [<package>=][github::]<username>/<repo>[/<subdir>]
#'     [@committish | #<pull> | @[*]release]
#'     }
#'
#' @section Resolution table:
#'
#' The data frame returned by `$resolve()` et al. has the following
#' columns:
#' * `ref` The original remote reference.
#' * `direct` Whether the reference was specified (`TRUE`), or it is a
#'   dependency (`FALSE`).
#' * `status` Status of the resolution: `"OK"` or `"FAILED"`.
#' * `package` Name of the package.
#' * `version` Package version.
#' * `platform` Platform: `"source"`, `"macos"` or `"windows"` currently.
#' * `rversion` If it is a binary package, which R version it was built for.
#'   For source packages `"*"` is used.
#' * `repodir` The relative directory of the package file in the repository.
#' * `sources` URLs to download the package from. CRAN URLs are not stable,
#'   so each row typically contains multiple URLs here.
#' * `target` Relative path for the package file in the repository.
#' * `fulltarget` Fulle path for the package file in the cache.
#'
#' @section Configuration options:
#' * `cache_dir` Path to the cache. By default a temporary directory is
#'   used. (TODO)
#' * `platforms` The platforms to use. Defaults to the current platform
#'   on Windows and macOS and source.
#' * `cran-mirror` The CRAN mirror to use. Defaults to `getOption("repos")`
#'   or the cloud mirror if not set.
#' * `dependencies` Dependencies to install. Defaults to the *hard*
#'   dependencies: `Depends`, `Imports`, `LinkingTo`.
#' * `r-versions`: R version to support, for the binary packages. Defaults
#'   to the running R version.
#'
#' @importFrom R6 R6Class
#' @import async
#' @name remotes
#' @examples
#' rems <- remotes$new(c("dplyr", "r-lib/rcmdcheck"))
#' rems$resolve()
#' rems$download()
#' rems$get_download_status()
NULL

#' @export

remotes <- R6Class(
  "remotes",
  public = list(
    initialize = function(specs, config = list())
      remotes_init(self, private, specs, config),
    async_resolve = function()
      remotes_async_resolve(self, private),
    resolve = function()
      remotes_resolve(self, private),
    get_resolution = function()
      remotes_get_resolution(self, private),
    get_download_status = function()
      remotes_get_download_status(self, private),
    async_download = function()
      remotes_async_download(self, private),
    download = function()
      remotes_download(self, private)
  ),

  private = list(
    dirty = FALSE,
    remotes = list(),
    resolution = NULL,
    downloads = NULL,
    download_cache = NULL,
    config = NULL,

    start_new_resolution = function()
      remotes__start_new_resolution(self, private),
    resolve_ref = function(rem, pool)
      remotes__resolve_ref(self, private, rem, pool),
    resolution_to_df = function()
      remotes__resolution_to_df(self, private),
    is_resolving = function(ref)
      remotes__is_resolving(self, private, ref),
    download_res = function(res)
      remotes_download_res(self, private, res)
  )
)

#' @importFrom utils modifyList

remotes_init <- function(self, private, specs, config) {
  private$remotes <- parse_remotes(specs)
  private$config <- modifyList(remotes_default_config(), config)
  mkdirp(private$download_cache <- private$config$cache_dir)
  private$dirty <- TRUE
  invisible(self)
}

remotes_default_config <- function() {
  list(
    "cache_dir"    = tempfile(),
    "platforms"    = unique(c(current_r_platform(), "source")),
    "cran-mirror"  = default_cran_mirror(),
    "dependencies" = c("Depends", "Imports", "LinkingTo"),
    "r-versions"   = current_r_version()
  )
}
