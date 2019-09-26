
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
#' @eval style_man()

new_pkg_download_proposal <- function(refs, ...) {
  pkg_download_proposal$new(refs, ...)
}

#' R6 class for package downloads
#'
#' Download packages with their dependencies, from various sources.
#' @eval style_man()
#'
#' @details
#' Typical workflow to download a set of packages:
#'
#' 1. Create a `pkg_download_proposal` object with
#'    `new_pkg_download_proposal()`.
#' 1. Resolve all possible dependencies with
#'    [`pkg_download_proposal$resolve()`](#method-resolve).
#' 1. Download all files with
#'    [`pkg_download_proposal$download()`](#method-download).
#' 1. Get the data about the packages and downloads with
#'    [`pkg_download_proposal$get_downloads()`](#method-get-downloads).
#'
#' @export

pkg_download_proposal <- R6::R6Class(
  "pkg_download_proposal",
  public = list(

    #' @details
    #' Create a new `pkg_download_proposal` object. Consider using
    #' `new_pkg_download_proposal()` instead of calling the constructor
    #' directly.
    #'
    #' The returned object can be used to look up (recursive) dependencies
    #' of R packages from various sources, and then to download the
    #' package files.
    #'
    #' @param refs Package names or references. See
    #'   ['Package references'][pkg_refs] for the syntax.
    #' @param config Configuration options, a named list. See
    #'   ['Configuration'][pkg_config].
    #' @param remote_types Custom remote ref types, this is for advanced
    #'   use, and experimental currently.
    #'
    #' @examples
    #' pdl <- pkg_download_proposal$new("r-lib/pkgdepends")
    #' pdl

    initialize = function(refs, config = list(), remote_types = NULL)
      pkgdl_init(self, private, refs, config, remote_types),

    #' @details
    #' The package refs that were used to create the
    #' `pkg_download_proposal` object.
    #'
    #' @return
    #' A character vector of package refs that were used to create the
    #' `pkg_download_proposal` object.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal(c("pak", "jsonlite"))
    #' pdl$get_refs()

    get_refs = function() private$plan$get_refs(),

    #' @details
    #' Configuration options for the `pkg_download_proposal` object. See
    #' ['Configuration'][pkg_config] for details.
    #'
    #' @return
    #' Named list. See ['Configuration'][pkg_config] for the configuration
    #' options.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal("pak")
    #' pdl$get_config()

    get_config = function() private$plan$get_config(),

    #' @details
    #' Resolve the dependencies of the specified package references. This
    #' usually means downloading metadata from CRAN and Bioconductor,
    #' unless already cached, and also from GitHub if GitHub refs were
    #' included, either directly or indirectly. See
    #' ['Dependency resolution'][pkg_resolution] for details.
    #'
    #' @return
    #' The same as the [`get_resolution()`](#method-get-resolution) method
    #' (see below), the result of the resolution, invisibly.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal("pak")
    #' pdl$resolve()
    #' pdl

    resolve = function()
      pkgdl_resolve(self, private),

    #' @details
    #' The same as [`resolve()`](#method-resolve), but asynchronous.
    #' This method is for advanced use.
    #'
    #' @return
    #' A deferred value.

    async_resolve = function()
      pkgdl_async_resolve(self, private),

    #' @details
    #' Query the result of the dependency resolution. This method can be
    #' called after [`resolve()`](#method-resolve) has completed.
    #'
    #' @return
    #' A [pkg_resolution_result] object, which is also a tibble. See
    #' [pkg_resolution_result] for its columns.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal("r-lib/pkgdepends")
    #' pdl$resolve()
    #' pdl$get_resolution()

    get_resolution = function()
      pkgdl_get_resolution(self, private),

    #' @details
    #' Download all resolved packages. It uses the package cache in the
    #' pkgcache package by default, to avoid downloads if possible.
    #'
    #' @return
    #' The same as the [`get_downloads()`](#method-get-downloads) method,
    #' the result, invisibly.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal("r-lib/pkgdepends")
    #' pdl$resolve()
    #' pdl$download()
    #' pdl

    download = function()
      pkgdl_download(self, private),

    #' @details
    #' The same as [`download()`](#method-download), but asynchronous.
    #' This method is for advanced use.
    #'
    #' @return
    #' A deferred value.

    async_download = function()
      pkgdl_async_download(self, private),

    #' @details
    #' Returns the summary of the package downloads.
    #'
    #' @return
    #' A [pkg_download_result] object, which is a list. See
    #' [pkg_download_result] for details.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal("pkgload")
    #' pdl$resolve()
    #' pdl$download()
    #' pdl$get_downloads()

    get_downloads = function()
      pkgdl_get_downloads(self, private),

    #' @details
    #' Format a `pkg_download_proposal` object, typically for printing.
    #'
    #' @param ... not used currently.
    #' @return
    #' A character vector, each element should be a line in the printout.

    format = function(...) pkgdl_format(self, private, ...),

    #' @details
    #' Prints a `pkg_download_proposal` object to the screen. The printout
    #' includes:
    #'
    #' * The package refs.
    #' * Whether the object has the resolved dependencies.
    #' * Whether the resolution had errors.
    #' * Whether the downloads were completed.
    #' * Whether the downloads had errors.
    #' * Advice on which methods to call next.
    #'
    #' See the example below.
    #'
    #' @param ... not used currently.
    #' @return
    #' The `pkg_download_proposal` object itself, invisibly.
    #'
    #' @examples
    #' pdl <- new_pkg_download_proposal("r-lib/pkgdepends")
    #' pdl
    #'
    #' pdl$resolve()
    #' pdl
    #'
    #' pdl$download()
    #' pdl

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
