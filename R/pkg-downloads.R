
#' Create a package download proposal
#'
#' A package download proposal can be used to query package dependencies
#' and/or download package files.
#'
#' Typical workflow to download a set of packages:
#' ```
#' pkgdl <- new_pkg_download_proposal(refs)
#' pkgdl$resolve()
#' pkgdl$download()
#' pkgdl$get_downloads()
#' ```
#'
#' @param refs Package names or references. See [pkg_refs] for the
#' syntax.
#'
#' @seealso [pkg_download_proposal] for the methods returned R6 object.
#' @export
#' @examples
#' \donttest{
#' dl <- new_pkg_download_proposal("r-lib/pak")
#' dl
#' dl$resolve()
#' dl$get_resolution()[]
#' dl$download()
#' dl$get_downloads()[]
#' }

new_pkg_download_proposal <- function(refs) {
  pkg_download_proposal$new(refs)
}

#' A package download proposal
#'
#' Typical workflow to download a set of packages:
#' ```
#' pkgdl <- new_pkg_download_proposal(refs)
#' pkgdl$resolve()
#' pkgdl$download()
#' pkgdl$get_downloads()
#' ```
#'
#' @section Usage:
#' ```
#' dl <- new_pkg_download_proposal(refs)
#' dl <- pkg_download_proposal$new(refs, config = list(), remote_types = NULL)
#'
#' dl$get_refs()
#' dl$get_config()
#'
#' dl$resolve()
#' dl$async_resolve()
#' dl$get_resolution()
#'
#' dl$download()
#' dl$async_download()
#' dl$get_downloads()
#' ```
#'
#' @section Arguments:
#' * `refs`: Package names or references. See [pkg_refs] for the syntax.
#' * `config`: Configuration options, a named list. See [pkg_config].
#' * `remotes_types`: Custom remote ref types, this is for advanced use,
#'   and experimental currently.
#'
#' @section Details:
#'
#' [new_pkg_download_proposal()] and `$new()` create a new package download
#' proposal. The latter has more options, and it is for expert use.
#'
#' `$get_refs()` returns the refs of a download proposal, see [pkg_refs].
#'
#' `$get_config()` returns the configuration, see [pkg_config].
#'
#' `$resolve()` resolves all dependencies of the remote references of the
#' download proposal. This usually involves downloading metadata from CRAN
#' and Bioconductor (unless already cached), and also from Github, if GitHub
#' refs are specified, either directly or indirectly. See [pkg_resolution].
#'
#' `$async_resolve()` is the asynchronous version of `$resolve()`, it
#' uses deferred values, and it is currently for advanced use.
#'
#' `$get_resolution()` returns the result of the resolution, in a
#' `pkg_resolution_result` object, which is also a tibble. See
#' [pkg_resolution_result] for the format.
#'
#' `$download()` downloads all resolved packages. It uses
#' the package cache in the pkgcache package by default, to avoid downloads
#' if possible.
#'
#' `$async_download()` downloads all resolved packages,
#' asynchronously. It returns a deferred value, and it is currently for
#' advanced use.
#'
#' `$get_downloads()` returns data about the downloaded packages.
#' See the format at [pkg_download_result].
#'
#' @name pkg_download_proposal
NULL

#' @export

pkg_download_proposal <- R6::R6Class(
  "pkg_download_proposal",
  public = list(
    initialize = function(refs, config = list(), remote_types = NULL)
      pkgdl_init(self, private, refs, config, remote_types),
    get_refs = function() private$plan$get_refs(),
    get_config = function() private$plan$get_config(),
    async_resolve = function()
      pkgdl_async_resolve(self, private),
    resolve = function()
      pkgdl_resolve(self, private),
    get_resolution = function()
      pkgdl_get_resolution(self, private),
    async_download = function()
      pkgdl_async_download(self, private),
    download = function()
      pkgdl_download(self, private),
    get_downloads = function()
      pkgdl_get_downloads(self, private),
    format = function(...) pkgdl_format(self, private, ...),
    print = function(...) pkgdl_print(self, private, ...)
  ),

  private = list(
    plan = NULL
  )
)

pkgdl_init <- function(self, private, refs, config, remote_types) {
  private$plan <- pkg_plan$new(refs, config, library = NULL, remote_types)
}

pkgdl_async_resolve <- function(self, private) {
  private$plan$async_resolve()
}

pkgdl_resolve <- function(self, private) {
  invisible(private$plan$resolve())
}

pkgdl_get_resolution <- function(self, private) {
  private$plan$get_resolution()
}

pkgdl_async_download <- function(self, private) {
  private$plan$async_download_resolution()
}

pkgdl_download <- function(self, private) {
  invisible(private$plan$download_resolution())
}

pkgdl_get_downloads <- function(self, private) {
  private$plan$get_resolution_download()
}

pkgdl_format <- function(self, private, ...) {
  refs <- private$plan$get_refs()

  has_res <- private$plan$has_resolution()
  res <- if (has_res) private$plan$get_resolution()
  res_err <- has_res && any(res$status != "OK")

  has_dls <- private$plan$has_resolution_downloads()
  dls <- if (has_dls) private$plan$get_resolution_download()
  dls_err <- has_dls && any(dls$status == "Failed")

  deps <- if (has_res) length(unique(res$package[!res$direct]))

  c("<pkg_download_proposal>",
    "+ refs:", paste0("  - ", refs),
    if (has_res) paste0("+ has resolution (+", deps, " dependencies)"),
    if (res_err) "x has resolution errors",
    if (has_dls) "+ has downloads",
    if (dls_err) "x has download errors",
    if (!has_res) "(use `$resolve()` to resolve dependencies)",
    if (has_res && !res_err && !has_dls)
      "(use `$download()` to download packages)",
    if (has_res) "(use `$get_resolution()` to see resolution results)",
    if (has_dls) "(use `$get_downloads()` to get download data)"
  )
}

pkgdl_print <- function(self, private, ...) {
  cat(self$format(...), sep = "\n")
}
