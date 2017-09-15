
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

    start_new_resolution = function()
      remotes__start_new_resolution(self, private),
    resolve_ref = function(rem)
      remotes__resolve_ref(self, private, rem),
    resolution_to_df = function()
      remotes__resolution_to_df(self, private),
    is_resolving = function(ref)
      remotes__is_resolving(self, private, ref),

    get_download_cache_dir = function()
      private$download_cache
  )
)

#' @importFrom rematch2 re_match

remotes_init <- function(self, private, specs) {
  mkdirp(private$download_cache <- tempfile())
  new_remotes <- parse_remotes(specs)
  private$remotes <- c(private$remotes, new_remotes)
  private$dirty <- TRUE
  invisible(self)
}
