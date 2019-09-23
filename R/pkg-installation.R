
#' @param refs Package names or references. See
#'   ['Package references'][pkg_refs] for the syntax.
#' @param config Configuration options, a named list. See
#'  ['Configuration'][pkg_config]. It needs to include the package library
#'  to install to, in `library`.
#' @param ... Additional arguments, passed to
#'   [`pkg_installation_proposal$new()`](#method-new).
#'
#' @details
#' `new_pkg_installation_proposal()` creates a new object from the
#' `pkg_installation_proposal` class. The advantage of
#' `new_pkg_installation_proposal()` compared to using the
#' [pkg_installation_proposal] constructor directly is that it avoids
#' making pkgdepends a build time dependency.
#'
#' @export
#' @rdname pkg_installation_proposal

new_pkg_installation_proposal <- function(refs, config = list(), ...) {
  config$library <- config$library %||% .libPaths()[[1]]
  pkg_installation_proposal$new(refs, config = config, ...)
}

#' @title
#' R6 class for package download and installation.
#'
#' @description
#' Download and installa packages, with their dependencies, from various
#' sources.
#' @eval style_man()
#'
#' @includeRmd tools/doc/pkg-installation.Rmd
#' @name pkg_installation_proposal
NULL

#' @export

pkg_installation_proposal <- R6::R6Class(
  "pkg_installation_proposal",
  public = list(
    initialize = function(refs, config = list(),
                          policy = c("lazy", "upgrade"),
                          remote_types = NULL)
      pkginst_init(self, private, refs, config, match.arg(policy),
                   remote_types),
    get_refs = function() private$plan$get_refs(),
    get_config = function() private$plan$get_config(),

    async_resolve = function()
      pkginst_async_resolve(self, private),
    resolve = function()
      pkginst_resolve(self, private),
    get_resolution = function()
      pkginst_get_resolution(self, private),

    get_solve_policy = function() private$policy,
    set_solve_policy = function(policy = c("lazy", "upgrade"))
      pkginst_set_solve_policy(self, private, match.arg(policy)),
    solve = function()
      pkginst_solve(self, private),
    get_solution = function()
      pkginst_get_solution(self, private),
    stop_for_solution_error = function()
      pkginst_stop_for_solution_error(self, private),
    draw = function()
      pkginst_draw(self, private),

    async_download = function()
      pkginst_async_download(self, private),
    download = function()
      pkginst_download(self, private),
    get_downloads = function()
      pkginst_get_downloads(self, private),
    stop_for_download_error = function()
      pkginst_stop_for_download_error(self, private),

    install = function()
      pkginst_install(self, private),
    get_install_plan = function()
      pkginst_get_install_plan(self, private),

    format = function(...) pkginst_format(self, private, ...),
    print = function(...) pkginst_print(self, private, ...)
  ),

  private = list(
    plan = NULL,
    policy = NULL,
    library = NULL
  )
)

pkginst_init <- function(self, private, refs, config, policy,
                         remote_types) {
  assert_that(is_path(config$library))
  private$library <- config$library
  private$policy <- policy
  private$plan <- pkg_plan$new(refs, config = config,
                               library = config$library,
                               remote_types = remote_types)
}

pkginst_async_resolve <- function(self, private) {
  private$plan$async_resolve()
}

pkginst_resolve <- function(self, private) {
  invisible(private$plan$resolve())
}

pkginst_get_resolution <- function(self, private) {
  private$plan$get_resolution()
}

pkginst_set_solve_policy <- function(self, private, policy) {
  if (private$policy != policy) {
    private$plan$delete_solution()
    private$policy <- policy
  }
}

pkginst_solve <- function(self, private) {
  invisible(private$plan$solve(policy = private$policy))
}

pkginst_get_solution <- function(self, private) {
  private$plan$get_solution()
}

pkginst_stop_for_solution_error <- function(self, private) {
  private$plan$stop_for_solve_error()
}

pkginst_draw <- function(self, private) {
  private$plan$draw_solution_tree()
}

pkginst_async_download <- function(self, private) {
  private$plan$async_download_solution()
}

pkginst_download <- function(self, private) {
  invisible(private$plan$download_solution())
}

pkginst_get_downloads <- function(self, private) {
  private$plan$get_solution_download()
}

pkginst_stop_for_download_error <- function(self, private) {
  private$plan$stop_for_solution_download_error()
}

pkginst_install <- function(self, private) {
  plan <- private$plan$get_install_plan()
  nw <- get_num_workers()
  install_package_plan(plan, lib = private$library, num_workers = nw)
}

pkginst_get_install_plan <- function(self, private) {
  private$plan$get_install_plan()
}

pkginst_format <- function(self, private, ...) {
  refs <- private$plan$get_refs()

  has_dls <- private$plan$has_solution_downloads()
  dls <- if (has_dls) private$plan$get_solution_download()
  dls_err <- has_dls && any(dls$status == "Failed")

  has_sol <- private$plan$has_solution()
  sol <- if (has_sol) private$plan$get_solution()
  sol_err <- has_sol && sol$status != "OK"

  c("<pkg_installation_proposal>",
    "+ refs:", paste0("  - ", refs),
    paste0("+ solution policy: ", private$policy),
    if (has_sol) "+ has solution",
    if (sol_err) "x has solution errors",
    if (has_dls) "+ has downloads",
    if (dls_err) "x has download errors",
    if (!has_sol) "(use `$solve()` to solve dependencies)",
    if (has_sol && !sol_err && !has_dls)
      "(use `$download()` to download packages)",
    if (has_sol) "(use `$get_solution()` to see solution results)",
    if (has_sol && !sol_err) "(use `$draw()` to draw the dependency tree)",
    if (has_dls) "(use `$get_downloads()` to get download data)",
    if (has_dls) "(use `$get_install_plan()` to get the installation plan)",
    if (has_dls) "(use `$install()` to install the packages)"
    )
}

pkginst_print <- function(self, private, ...) {
  cat(self$format(...), sep = "\n")
}
