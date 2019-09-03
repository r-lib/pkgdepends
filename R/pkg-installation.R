
#' Create a package installation proposal
#'
#' A package installation proposal can be used to install or upgrade
#' a set of packages in a package library.
#'
#' Typical workflow to install a set of packages:
#' ```
#' inst <- new_pkg_installation_proposal(refs, library)
#' inst$solve()
#' inst$download()
#' inst$install()
#' ```
#'
#' @param refs Package names or references. See [pkg_refs] for the
#' syntax.
#' @param library Path to package library to install to. If it does not
#' exist, it will be created.
#' @param upgrade Whether to update packages (including dependencies)
#' to their latest available version.
#' @param dependencies Whether dependent packages should be installed,
#' and which ones. Defaults to installing the required (recursive)
#' dependencies.
#'
#' @seealso [pkg_installation_proposal] for the methods of the returned R6
#' object.
#' @examples
#' \donttest{
#' inst <- new_pkg_installation_proposal("r-lib/cli", library = tempfile())
#' inst
#' inst$solve()
#' inst$draw()
#' inst$download()
#' inst$get_install_plan()
#' inst$install()
#' }

new_pkg_installation_proposal <- function(refs, library, upgrade = FALSE,
                                          dependencies = NA) {
  policy <- if (upgrade) "upgrade" else "lazy"
  config <- list(
    library = library,
    dependencies = dependencies)
  pkg_installation_proposal$new(refs, config = config, policy = policy,
                                remote_types = NULL)
}

#' A package installation proposal
#'
#' Typical workflow to install a set of packages:
#' ```
#' inst <- new_pkg_installation_proposal(refs, library)
#' inst$resolve()
#' inst$solve()
#' inst$download()
#' inst$install()
#' ```
#'
#' @section Usage:
#' ```
#' inst <- new_pkg_installation_proposal(refs, library, upgrade = FALSE,
#'                              dependencies = NA)
#' inst <- pkg_installation_proposal$new(refs, config = list(),
#'                              policy = c("lazy", "upgrade"),
#'                              remote_types = NULL)
#'
#' inst$get_refs()
#' inst$get_config()
#'
#' inst$resolve()
#' inst$async_resolve()
#' inst$get_resolution()
#'
#' inst$get_solve_policy()
#' inst$set_solve_policy(policy = c("lazy", "upgrade"))
#' inst$solve()
#' inst$get_solution()
#' inst$stop_for_solution_error()
#' inst$draw()
#'
#' inst$download()
#' inst$async_download()
#' inst$get_downloads()
#' inst$stop_for_download_error()
#'
#' inst$get_install_plan()
#' inst$install()
#' ```
#'
#' @section Arguments:
#' * `refs`: Package names or references. See [pkg_refs] for the syntax.
#' * `library`: Path to package library to install the packages to. If it
#'   does not exist, it will be created.
#' * `upgrade`: Whether to update packages (including dependencies)
#'   to their latest available version.
#' * `dependencies`: Whether dependent packages should be installed,
#'   and which ones. Defaults to installing the required (recursive)
#'   dependencies.
#' * `config`: Configuration options, a named list. See [pkg_config].
#' * `policy`: Solution policy. See [pkg_solve].
#' * `remotes_types`: Custom remote ref types, this is for advanced use,
#'   and experimental currently.
#'
#' @section Details:
#'
#' [new_pkg_installation_proposal()] and `pkg_installation_proposal$new()`
#' both create a new package installation proposal object. The latter has
#' more options and it is for expert use.
#'
#' `$get_refs()` returns the refs of an installation proposal,
#' see [pkg_refs].
#'
#' `$get_config()` returns the configuration, see [pkg_config].
#'
#' `$resolve()` resolves all dependencies of the remote references of the
#' installation proposal. This usually involves downloading metadata from
#' CRAN and Bioconductor (unless already cached), and also from Github, if
#' GitHub refs are specified, either directly or indirectly. See
#' [pkg_resolution].
#'
#' `$async_resolve()` is the asynchronous version of `$resolve()`, it
#' uses deferred values, and it is currently for advanced use.
#'
#' `$get_resolution()` returns the result of the resolution, in a
#' `pkg_resolution_result` object, which is also a tibble. See
#' [pkg_resolution_result] for the format.
#'
#' `$get_solve_policy()` returns the policy of the dependency solver.
#' See [pkg_solve].
#'
#' `$set_solve_policy()` sets the policy of the dependency solver. If this
#' policy changes, then you need to run `solve()` again. See [pkg_solve].
#'
#' `$solve()` solves the package dependencies to work out the which
#' of the available versions of each needs to be installed to have a
#' properly working set of packages. See [pkg_solve].
#'
#' `$get_solution()` returns the solution of the installation problem.
#'
#' `$stop_for_solution_error()` throws an error, with a meaningful error
#' message, if the previous `$solve()` call failed.
#'
#' `$draw()` draws the package dependency tree of a solution. It only
#' works once `solve()` was called.
#'
#' `$download()` downloads all packages of the solution. It uses
#' the package cache in the pkgcache package by default, to avoid downloads
#' if possible.
#'
#' `$async_download()` downloads all packages of the solution,
#' asynchronously. It returns a deferred value, and it is currently for
#' advanced use.
#'
#' `$get_downloads()` returns data about the downloaded packages.
#' See the format at [pkg_download_result].
#'
#' `$stop_for_download_error()` throws an error for a package download
#' error.
#'
#' `$get_install_plan()` returns the package installation plan of the
#' proposal.
#'
#' `$install()` performs the package installation plan.
#'
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
  private$plan$stop_for_solution_error()
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
    if (has_sol) "(use `$draw()` to draw the dependency tree)",
    if (has_dls) "(use `$get_downloads()` to get download data)",
    if (has_dls) "(use `$get_install_plan()` to get the installation plan)",
    if (has_dls) "(use `$install()` to install the packages)"
    )
}

pkginst_print <- function(self, private, ...) {
  cat(self$format(...), sep = "\n")
}
