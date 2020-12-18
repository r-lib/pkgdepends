
pkg_plan <- R6::R6Class(
  "pkg_plan",
  public = list(
    initialize = function(refs, config = list(), library = NULL,
                          remote_types = NULL, lockfile = NULL)
      pkgplan_init(self, private, refs, config, library, remote_types,
                   lockfile),

    get_refs = function() private$refs,
    has_resolution = function() !is.null(private$resolution$result),
    has_clean_resolution = function()
      self$has_resolution() && (all(private$resolution$result$status == "OK")),
    has_resolution_downloads = function() !is.null(private$downloads),
    has_solution_downloads = function() !is.null(private$solution_downloads),
    has_solution = function() !is.null(private$solution),
    get_config = function() private$config,

    async_resolve = function()
      pkgplan_async_resolve(self, private),
    resolve = function()
      pkgplan_resolve(self, private),
    get_resolution = function()
      pkgplan_get_resolution(self, private),

    async_download_resolution = function()
      pkgplan_async_download_resolution(self, private),
    download_resolution = function()
      pkgplan_download_resolution(self, private),
    get_resolution_download = function()
      pkgplan_get_resolution_download(self, private),

    solve = function(policy = c("lazy", "upgrade"))
      pkgplan_solve(self, private, match.arg(policy)),
    delete_solution = function()
      private$solution <- NULL,
    stop_for_solve_error = function()
      pkgplan_stop_for_solve_error(self, private),
    get_solution = function()
      pkgplan_get_solution(self, private),
    show_solution = function(key = FALSE)
      pkgplan_show_solution(self, private, key),
    get_install_plan = function()
      pkgplan_install_plan(self, private, downloads = TRUE),
    export_install_plan = function(plan_file = "pkg.lock", version = 1)
      pkgplan_export_install_plan(self, private, plan_file, version),
    draw_solution_tree = function(pkgs = NULL, annotate = TRUE)
      pkgplan_draw_solution_tree(self, private, pkgs, annotate),

    async_download_solution = function()
      pkgplan_async_download_solution(self, private),
    download_solution = function()
      pkgplan_download_solution(self, private),
    get_solution_download = function()
      pkgplan_get_solution_download(self, private),
    stop_for_solution_download_error = function()
      pkgplan_stop_for_solution_download_error(self, private),
    stop_for_resolution_download_error = function()
      pkgplan_stop_for_resolution_download_error(self, private),

    print = function(...)
      pkgplan_print(self, private, ...)
  ),

  private = list(
    refs = NULL,
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
      pkgplan_download_res(self, private, res, which, on_progress),
    subset_resolution = function(which)
      pkgplan__subset_resolution(self, private, which),
    create_lp_problem = function(pkgs, policy, rversion)
      pkgplan__create_lp_problem(self, private, pkgs, policy, rversion),
    solve_lp_problem = function(problem)
      pkgplan__solve_lp_problem(self, private, problem),

    create_progress_bar = function(what) {
      bar <- pkgplan__create_progress_bar(what)
      pkgplan__init_progress_bar(bar)
      bar
    },
    update_progress_bar = function(idx, event, data)
      pkgplan__update_progress_bar(private$progress_bar, idx, event, data),
    done_progress_bar = function() {
      if (!is.null(private$progress_bar)) {
        pkgplan__done_progress_bar(private$progress_bar)
        private$progress_bar <- NULL
      }
    }
  )
)

#' @importFrom utils modifyList

pkgplan_init <- function(self, private, refs, config, library,
                         remote_types, lockfile) {

  if (!is.null(lockfile)) {
    return(pkgplan_init_lockfile(
      self,
      private,
      lockfile,
      config,
      library,
      remote_types
    ))
  }

  assert_that(is_character(refs),
              is_valid_config(config),
              is_path_or_null(library))

  private$refs <- refs
  private$remotes <- parse_pkg_refs(refs)
  private$config <- modifyList(pkgplan_default_config(), config)
  private$config$library <- library
  private$remote_types <- remote_types %||% default_remote_types()

  if (!is.null(library)) {
    mkdirp(library, msg = "Creating library directory")
    library <- normalizePath(library)
  }
  mkdirp(private$download_cache <- private$config$cache_dir)

  installed <- NULL
  if (!is.null(library)) {
    installed <- merge_installed_caches(
      make_installed_cache(library),
      make_installed_cache(.Library, priority = "recommended")
    )
  }

  private$cache <- list(
    metadata = pkgcache::cranlike_metadata_cache$new(
      replica_path = private$config$metadata_cache_dir,
      platforms = private$config$platforms,
      r_version = private$config$`r-versions`,
      cran_mirror = private$config$`cran-mirror`,
      update_after = private$config$`metadata-update-after`),
    package = pkgcache::package_cache$new(private$config$package_cache_dir),
    installed = installed
  )

  private$dirty <- TRUE
  invisible(self)
}

pkgplan_init_lockfile <- function(self, private, lockfile, config,
                                   library, remote_types) {
  assert_that(
    is_path(lockfile),
    is_valid_config(config),
    is_path_or_null(library)
  )

  private$config <- modifyList(pkgplan_default_config(), config)
  private$config$library <- library
  private$remote_types <- remote_types %||% default_remote_types()

  if (!is.null(library)) {
    mkdirp(library, msg = "Creating library directory")
    library <- normalizePath(library)
  }
  mkdirp(private$download_cache <- private$config$cache_dir)

  raw <- fromJSON(readLines(lockfile), simplifyVector = FALSE)
  if (raw$lockfile_version != 1) {
    stop("Unknown lockfile version: ", raw$lockfile_version)
  }

  pkgs <- raw$packages
  refs <- vcapply(pkgs, "[[", "ref")
  soldata <- tibble(
    ref              = refs,
    type             = vcapply(pkgs, "[[", "type"),
    direct           = vlapply(pkgs, "[[", "direct"),
    directpkg        = vlapply(pkgs, "[[", "directpkg"),
    status           = "OK",
    package          = vcapply(pkgs, "[[", "package"),
    version          = vcapply(pkgs, "[[", "version"),
    license          = vcapply(pkgs, "[[", "license"),
    needscompilation = vlapply(pkgs, function(x) x$needscompilation %||% NA),
    sha256           = vcapply(pkgs, function(x) x$sha256 %||% NA_character_),
    filesize         = viapply(pkgs, function(x) x$filesize %||% NA_integer_),
    built            = vcapply(pkgs, function(x) x$built %||% NA_character_),
    platform         = vcapply(pkgs, "[[", "platform"),
    rversion         = vcapply(pkgs, "[[", "rversion"),
    target           = vcapply(pkgs, "[[", "target"),
    dependencies     = lapply(pkgs, function(x) unlist(x$dependencies) %||% character()),
    sources          = lapply(pkgs, function(x) unlist(x$sources)),
    metadata         = lapply(pkgs, function(x) unlist(x$metadata)),
    dep_types        = lapply(pkgs, function(x) unlist(x$dep_types)),
    remote           = parse_pkg_refs(refs),
    cache_status     = "miss",
    lib_status       = "new",
    old_version      = NA_character_,
    new_version      = version,
    extra            = list(list())

  )

  private$refs <- refs[soldata$direct]
  private$remotes <- soldata$remote
  private$dirty <- FALSE

  private$cache <- list(
    metadata = NULL,
    package = pkgcache::package_cache$new(private$config$package_cache_dir),
    installed = NULL
  )

  private$resolution <- list(result = soldata)
  private$solution <- list(
    result = structure(
      class = c("pkg_solution_result", "list"),
      list(
        status = "OK",
        data = soldata,
        problem = list(pkgs = soldata),
        solution = NULL
      )
    )
  )

  invisible(self)
}

#' Configuration
#'
#' Configuration options for several pkgdepends classes. Not all classes
#' use all of these options. E.g. a [`pkg_download_proposal`] is not concerned
#' about package libraries, so it'll ignore the `library` option.
#'
#' Options:
#'
#' * `library`: package library to use for checking already installed
#'   packages when considering dependencies in [dependency lookup][pkg_deps]
#'   or [package installation][pkg_installation_proposal]. Defaults to the
#'   first path in [.libPaths()].
#' * `cache_dir`: directory to download the packages to. Defaults to a
#'   temporary directory within the R session temporary directory, see
#'   [base::tempdir()].
#' * `package_cache_dir`: package cache location of
#'   [`pkgcache::package_cache`]. The default is the pkgcache default.
#' * `metadata_cache_dir`: location of metadata replica of
#'   [`pkgcache::cranlike_metadata_cache`]. Defaults to a temporary
#'   directory within the R session temporary directory, see
#'   [base::tempdir()].
#' * `platforms`: Character vector of platforms to _download_ or _install_
#'   for. Possible platforms are `windows`, `macos` and `source`. Defaults
#'   to the current platform, _and_ `source`.
#' * `cran-mirror`: CRAN mirror to use. Defaults to the `repos` option
#'   (see [base::options()]), if that's not set then
#'   `https://cran.rstudio.com`.
#' * `dependencies`: Dependencies to consider or download or install.
#'   Defaults to the hard dependencies, see [pkg_dep_types_hard()].
#' * `r-versions`: Character vector, R versions to download or install
#'   packages for. It defaults to the current R version.
#' * `build-vignettes`: Whether to build vignettes for package trees.
#'   This is only used if the package is obtained from a package tree,
#'   and not from a source (or binary) package archive. By default vignettes
#'   are not built in this case. If you set this to `TRUE`, then you need
#'   to make sure that the vignette builder packages are available, as
#'   these are not installed by default currently.
#' * `metadata-update-after`: A time interval as a [difftime] object.
#'   pkgdepends will update the metadata cache if it is older than this.
#'   The default is one day.
#' @name pkg_config
NULL

pkgplan_default_config <- function() {
  structure(list(
    "library"            = NULL,
    "cache_dir"          = detect_download_cache_dir(),
    "package_cache_dir"  = NULL,
    "metadata_cache_dir" = tempfile(),
    "platforms"          = default_platforms(),
    "cran-mirror"        = default_cran_mirror(),
    "dependencies"       = pkg_dep_types_hard(),
    "r-versions"         = current_r_version(),
    "build-vignettes"    = FALSE,
    "metadata-update-after" = as.difftime(24, units = "hours")
  ), class = "pkg_config")
}

#' @export

format.pkg_config <- function(x, ...) {

  format_dependencies <- function(dep) {
    dep <- as_pkg_dependencies(dep)
    paste0("direct: ", paste(dep$direct, collapse = ", "),
           ", indirect: ", paste(dep$indirect, collapse = ", "))
  }

  c("+ config:",
    paste0("  - library: ", x$library),
    paste0("  - cache_dir: ", x$cache_dir),
    paste0("  - package_cache_dir: ", x$package_cache_dir %||% "<default>"),
    paste0("  - metadata_cache_dir: ", x$metadata_cache_dir),
    paste0("  - platforms: ", paste(x$platforms, collapse = ", ")),
    paste0("  - cran-mirror: ", x$`cran-mirror`),
    paste0("  - dependencies: ", format_dependencies(x$dependencies)),
    paste0("  - r-versions: ", paste(x$`r-versions`, collapse = ", ")),
    paste0("  - build-vignettes: ", x$`build-vignettes`),
    paste0("  - metadata-update-after: ", format(x$`metadata-update-after`))
  )
}

#' @export

print.pkg_config <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

is_valid_config <- function(x) {
  assert_that(is.list(x), all_named(x))
  assert_that(all(names(x) %in% names(pkgplan_default_config())))
  for (n in names(x)) {
    switch (
      n,
      library            = assert_that(is_path(x[[n]])),
      cache_dir          = assert_that(is_path(x[[n]])),
      package_cache_dir  = assert_that(is_path(x[[n]])),
      metadata_cache_dir = assert_that(is_path(x[[n]])),
      platforms          = assert_that(is_platform_list(x[[n]])),
      "cran-mirror"      = assert_that(is_string(x[[n]])),
      dependencies       = assert_that(is_dependencies(x[[n]])),
      "r-versions"       = assert_that(is_r_version_list(x[[n]])),
      "build-vignettes"  = assert_that(is_flag(x[[n]])),
      "metadata-update-after" = assert_that(is_difftime(x[[n]]))
    )
  }
  TRUE
}

on_failure(is_valid_config) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid configuration list")
}

pkgplan_get_total_files <- function(self, private) {
  nrow(private$resolution$result)
}

pkgplan_print <- function(self, private, ...) {
  cat("<pkg_plan>\n")

  ## refs
  refs <- self$get_refs()
  cat(
    strwrap(
      paste0("- refs: ", paste(backtick(refs), collapse = ", ")),
      indent = 0, exdent = 4
    ),
    sep = "\n"
  )

  ## library
  if (!is.null(private$config$library)) {
    cat("- library:", backtick(private$config$library), "\n")
  }

  ## resolution
  if (self$has_resolution()) {
    if (self$has_clean_resolution()) {
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
