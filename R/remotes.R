
#' @importFrom R6 R6Class

remotes <- R6Class(
  "remotes",
  public = list(
    initialize = function(specs, config = list())
      remotes_init(self, private, specs, config),
    async_resolve = function()
      remotes_async_resolve(self, private),
    resolve = function()
      remotes_resolve(self, private),
    get_resolution = function()
      remotes_get_resolution(self, private),
    get_download_status = function()
      remotes_get_download_status(self, private),
    async_download = function()
      remotes_async_download(self, private),
    download = function()
      remotes_download(self, private)
  ),

  private = list(
    dirty = FALSE,
    remotes = list(),
    resolution = NULL,
    downloads = NULL,
    download_cache = NULL,
    config = NULL,

    start_new_resolution = function()
      remotes__start_new_resolution(self, private),
    resolve_ref = function(rem, pool)
      remotes__resolve_ref(self, private, rem, pool),
    resolution_to_df = function()
      remotes__resolution_to_df(self, private),
    is_resolving = function(ref)
      remotes__is_resolving(self, private, ref),
    download_res = function(res)
      remotes_download_res(self, private, res)
  )
)

#' @importFrom utils modifyList

remotes_init <- function(self, private, specs, config) {
  private$remotes <- parse_remotes(specs)
  private$config <- modifyList(remotes_default_config(), config)
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
