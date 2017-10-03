
#' Class for package dependency resolution and package downloads
#'
#' @section Usage:
#' ```
#' r <- remotes$new(specs, config = list())
#'
#' r$resolve()
#' r$async_resolve(progress_bar)
#' r$get_resolution()
#' r$draw_tree(pkgs)
#'
#' r$download()
#' r$async_download(progress_bar)
#' r$get_download_status()
#'
#' @section Arguments:
#' * `specs`: Package specifications. See 'Remote Types' below.
#' * `config`: Custom configuration, a named list. See the list of options
#'   below.
#' * `progress_bar` A [progress::progress_bar] object from the `progress`
#'   package. This is optional, and can be used to track progress. See
#'   details below.
#' * `pkgs`: Charcater vector of regular expressions, to specify the
#'   packages to query.
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
#' `$draw_tree()` draws the dependency tree of one or more packages,
#' after resolution.
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
#' @section Progress bars:
#' TODO
#'
#' @importFrom R6 R6Class
#' @import async
#' @name remotes
#' @examples
#' ## This does download a bunch of packages, so we don't run it currently
#' \dontrun{
#' rems <- remotes$new(c("dplyr", "r-lib/rcmdcheck"))
#' rems$resolve()
#' rems$download()
#' rems$get_download_status()
#' }
NULL

#' @export

remotes <- R6Class(
  "remotes",
  public = list(
    initialize = function(specs, config = list(), library = NULL)
      remotes_init(self, private, specs, config, library),

    async_resolve = function(progress_bar = NULL)
      remotes_async_resolve(self, private, progress_bar),
    resolve = function()
      remotes_resolve(self, private),
    get_resolution = function()
      remotes_get_resolution(self, private),
    draw_tree = function(pkgs = NULL)
      remotes_draw_tree(self, private, pkgs),

    async_download_resolution = function(progress_bar = NULL)
      remotes_async_download_resolution(self, private, progress_bar),
    download_resolution = function()
      remotes_download_resolution(self, private),
    get_resolution_download = function()
      remotes_get_resolution_download(self, private),

    solve = function()
      remotes_solve(self, private),
    get_solution = function()
      remotes_get_solution(self, private),

    async_download_solution = function(progress_bar = NULL)
      remotes_async_download_solution(self, private, progress_bar),
    download_solution = function()
      remotes_download_solution(self, private),
    get_solution_download = function()
      remotes_get_solution_download(self, private)
  ),

  private = list(
    library = NULL,
    dirty = FALSE,
    remotes = list(),
    resolution = NULL,
    solution = NULL,
    downloads = NULL,
    solution_downloads = NULL,
    download_cache = NULL,
    config = NULL,

    start_new_resolution = function(progress_bar)
      remotes__start_new_resolution(self, private, progress_bar),
    resolve_ref = function(rem, pool)
      remotes__resolve_ref(self, private, rem, pool),
    resolution_to_df = function(resolution)
      remotes__resolution_to_df(self, private, resolution),
    is_resolving = function(ref)
      remotes__is_resolving(self, private, ref),
    download_res = function(res)
      remotes_download_res(self, private, res),
    subset_resolution = function(which)
      remotes__subset_resolution(self, private, which),
    create_lp_problem = function(pkgs)
      remotes__create_lp_problem(self, private, pkgs),
    solve_lp_problem = function(problem)
      remotes__solve_lp_problem(self, private, problem)
  )
)

#' @importFrom utils modifyList

remotes_init <- function(self, private, specs, config, library) {
  private$remotes <- parse_remotes(specs)
  private$config <- modifyList(remotes_default_config(), config)
  private$library <- library
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

remotes_get_total_files <- function(self, private) {
  nrow(private$resolution$result)
}
