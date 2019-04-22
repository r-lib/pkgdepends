
#' Class for package dependency resolution and package downloads
#'
#' @section Usage:
#' ```
#' r <- remotes()$new(specs, config = list())
#'
#' r$resolve()
#' r$async_resolve()
#' r$get_resolution()
#' r$draw_tree(pkgs)
#'
#' r$download()
#' r$async_download()
#' r$get_download_status()
#'
#' @section Arguments:
#' * `specs`: Package specifications. See 'Remote Types' below.
#' * `config`: Custom configuration, a named list. See the list of options
#'   below.
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
#' @name remotes
#' @examples
#' ## This does download a bunch of packages, so we don't run it currently
#' \dontrun{
#' rems <- remotes()$new(c("dplyr", "r-lib/rcmdcheck"))
#' rems$resolve()
#' rems$download_resolution()
#' rems$get_download_status()
#' }
NULL

#' @export

remotes <- function() {
  R6::R6Class(
    "remotes",
    public = list(
      initialize = function(specs, config = list(), library = NULL,
                            remote_types = NULL)
        remotes_init(self, private, specs, config, library, remote_types),

      async_resolve = function()
        remotes_async_resolve(self, private),
      resolve = function()
        remotes_resolve(self, private),
      get_resolution = function()
        remotes_get_resolution(self, private),

      async_download_resolution = function()
        remotes_async_download_resolution(self, private),
      download_resolution = function()
        remotes_download_resolution(self, private),
      get_resolution_download = function()
        remotes_get_resolution_download(self, private),

      solve = function(policy = c("lazy", "upgrade"))
        remotes_solve(self, private, match.arg(policy)),
      stop_for_solve_error = function()
        remotes_stop_for_solve_error(self, private),
      get_solution = function()
        remotes_get_solution(self, private),
      get_install_plan = function(downloads = TRUE)
        remotes_install_plan(self, private, downloads),
      draw_tree = function(pkgs = NULL)
        remotes_draw_tree(self, private, pkgs),

      async_download_solution = function()
        remotes_async_download_solution(self, private),
      download_solution = function()
        remotes_download_solution(self, private),
      get_solution_download = function()
        remotes_get_solution_download(self, private),
      stop_for_solution_download_error = function()
        remotes_stop_for_solution_download_error(self, private),

      print = function(...)
        remotes_print(self, private, ...)
    ),

    private = list(
      library = NULL,
      dirty = FALSE,
      remotes = list(),
      cache = NULL,
      resolution = NULL,
      solution = NULL,
      downloads = NULL,
      solution_downloads = NULL,
      download_cache = NULL,
      config = NULL,
      progress_bar = NULL,
      progress_bar_timer = NULL,
      remote_types = NULL,

      download_res = function(res, which, on_progress = NULL)
        remotes_download_res(self, private, res, which, on_progress),
      subset_resolution = function(which)
        remotes__subset_resolution(self, private, which),
      create_lp_problem = function(pkgs, policy)
        remotes__create_lp_problem(self, private, pkgs, policy),
      solve_lp_problem = function(problem)
        remotes__solve_lp_problem(self, private, problem),

      create_progress_bar = function(what)
        remotes__create_progress_bar(self, private, what),
      update_progress_bar = function(idx, data)
        remotes__update_progress_bar(self, private, idx, data),
      show_progress_bar = function()
        remotes__show_progress_bar(self, private),
      done_progress_bar = function()
        remotes__done_progress_bar(self, private)
    )
  )
}

#' @importFrom utils modifyList

remotes_init <- function(self, private, specs, config, library,
                         remote_types) {

  assert_that(is_character(specs),
              is_valid_config(config),
              is_path_or_null(library))

  private$remotes <- parse_remotes(specs)
  private$config <- modifyList(remotes_default_config(), config)
  private$remote_types <- remote_types %||% default_remote_types()

  if (!is.null(library)) {
    mkdirp(library, msg = "Creating library directory")
    library <- normalizePath(library)
  }
  private$library <- library
  mkdirp(private$download_cache <- private$config$cache_dir)

  private$cache <- list(
    metadata = pkgcache::cranlike_metadata_cache$new(
      replica_path = private$config$metadata_cache_dir,
      platforms = private$config$platforms,
      r_version = private$config$`r-version`,
      cran_mirror = private$config$`cran-mirror`),
    package = pkgcache::package_cache$new(private$config$package_cache_dir),
    installed = if (!is.null(library)) make_installed_cache(library)
  )

  private$dirty <- TRUE
  invisible(self)
}

remotes_default_config <- function() {
  list(
    "cache_dir"          = detect_download_cache_dir(),
    "package_cache_dir"  = NULL,
    "metadata_cache_dir" = tempfile(),
    "platforms"          = default_platforms(),
    "cran-mirror"        = default_cran_mirror(),
    "dependencies"       = dep_types_hard(),
    "r-versions"         = current_r_version()
  )
}

is_valid_config <- function(x) {
  assert_that(is.list(x), all_named(x))
  assert_that(all(names(x) %in% names(remotes_default_config())))
  for (n in names(x)) {
    switch (
      n,
      cache_dir          = assert_that(is_path(x[[n]])),
      package_cache_dir  = assert_that(is_path(x[[n]])),
      metadata_cache_dir = assert_that(is_path(x[[n]])),
      platforms          = assert_that(is_platform_list(x[[n]])),
      "cran-mirror"      = assert_that(is_string(x[[n]])),
      dependencies       = assert_that(is_dependencies(x[[n]])),
      "r-versions"       = assert_that(is_r_version_list(x[[n]]))
    )
  }
  TRUE
}

on_failure(is_valid_config) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid configuration list")
}

remotes_get_total_files <- function(self, private) {
  nrow(private$resolution$result)
}

remotes_print <- function(self, private, ...) {
  cat("<remotes>\n")

  ## refs
  refs <- vcapply(private$remotes, "[[", "ref")
  cat(
    strwrap(
      paste0("- refs: ", paste(backtick(refs), collapse = ", ")),
      indent = 0, exdent = 4
    ),
    sep = "\n"
  )

  ## library
  if (!is.null(private$library)) {
    cat("- library:", backtick(private$library), "\n")
  }

  ## resolution
  if (!is.null(private$resolution$result)) {
    if (all(private$resolution$result$status == "OK")) {
      cat("- has resolution\n")
    } else {
      cat("- has resolution, with errors\n")
    }
  }

  ## solution
  if (!is.null(private$solution$result)) {
    if (private$solution$result$status == "OK") {
      cat("- has solution\n")
    } else {
      cat("- has solution, with errors\n")
    }
  }

  invisible(self)
}
