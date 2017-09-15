
#' @importFrom R6 R6Class

remotes <- R6Class(
  "remotes",
  public = list(
    initialize = function(specs)
      remotes_init(self, private, specs),
    resolve = function()
      remotes_resolve(self, private),
    get_resolution = function()
      remotes_get_resolution(self, private),
    get_download_status = function()
      remotes_get_download_status(self, private),

    download = function()
      remotes_download(self, private)
  ),

  private = list(
    dirty = FALSE,
    remotes = list(),
    resolution = NULL,
    downloads = NULL,
    download_cache = NULL,

    update_cache = function(type)
      remotes__update_cache(self, private, type),
    update_cache_cran = function()
      remotes__update_cache_cran(self, private),

    start_new_resolution = function()
      remotes__start_new_resolution(self, private),
    resolve_ref = function(rem)
      remotes__resolve_ref(self, private, rem),
    resolve_ref_cran = function(rem)
      remotes__resolve_ref_cran(self, private, rem),
    resolve_ref_cran_current = function(rem)
      remotes__resolve_ref_cran_current(self, private, rem),
    resolve_ref_cran_general = function(rem)
      remotes__resolve_ref_cran_general(self, private, rem),
    resolve_ref_cran_version_files = function(package, version,
      dependencies)
      remotes__resolve_ref_cran_version_files(self, private, package,
                                              version, dependencies),
    resolve_ref_github = function(rem)
      remotes__resolve_ref_github(self, private, rem),
    resolve_ref_github_deps = function(rem, deps, remotes, dependencies)
      remotes__resolve_ref_github_deps(self, private, rem, deps, remotes,
                                       dependencies),
    resolution_to_df = function()
      remotes__resolution_to_df(self, private),
    is_resolving = function(ref)
      remotes__is_resolving(self, private, ref),

    get_download_cache_dir = function()
      private$download_cache,

    download_cran = function(resolution)
      remotes__download_cran(self, private, resolution),
    download_github = function(resolution)
      remotes__download_github(self, private, resolution)
  )
)

remotes_init <- function(self, private) {
  mkdirp(private$download_cache <- tempfile())
  invisible(self)
}

#' Add remotes to manage
#'
#' We also parse them to make sure that they are valid,
#' but don't actually try to resolve them just yet.
#'
#' @param self Self.
#' @param private Private self.
#' @param specs Character vector, remotes specifications.
#'
#' @keywords internal
#' @importFrom rematch2 re_match

remotes_add_remotes <- function(self, private, specs) {
  new_remotes <- parse_remotes(specs)
  private$remotes <- c(private$remotes, new_remotes)
  private$cfg$set("packages", vcapply(private$remotes, "[[", "ref"))
  private$dirty <- TRUE
  invisible(self)
}

remotes_remove_remotes <- function(self, private, specs) {
  specs <- unique(specs)
  ex <- vcapply(private$remotes, "[[", "ref")
  nex <- setdiff(specs, ex)
  if (length(nex)) {
    warning("Unknown remotes: ", paste(sQuote(nex), collapse = ", "))
    specs <- setdiff(specs, nex)
  }

  torem <- specs %in% ex
  if (length(torem)) private$remotes <- private$remotes[-torem]

  private$cfg$set("packages", vcapply(private$remotes, "[[", "ref"))
  private$dirty <- TRUE
  invisible(self)
}

remotes__update_cache <- function(self, private, type) {
  private$resolution$cache[[type]] <-
    if (type == "cran") {
      private$update_cache_cran()
    } else if (type == "github") {
      ## Nothing to do
    }
}
