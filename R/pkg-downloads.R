
#' @param refs Package names or references. See
#'   ['Package references'][pkg_refs] for the syntax.
#' @param ... Additional arguments, passed to
#'   [`pkg_download_proposal$new()`]($method-new).
#'
#' @details
#' `new_pkg_download_proposal()` creates a new object from the
#' `pkg_download_proposal` class, that can be used to look up and download
#' R packages and their dependencies. The advantage of
#' `new_pkg_download_proposal()` compared to using the
#' [pkg_download_proposal] constructor directly is that it avoids making
#' pkgdepends a build time dependency.
#'
#' @export
#' @rdname pkg_download_proposal

new_pkg_download_proposal <- function(refs, ...) {
  pkg_download_proposal$new(refs, ...)
}

#' @title Package Downloads
#'
#' @description
#' Download packages with their dependencies, from various sources.
#' @eval style_man()
#'
#' @includeRmd tools/doc/pkg-download.Rmd
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
