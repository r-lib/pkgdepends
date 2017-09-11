
#' @importFrom R6 R6Class

remotes <- R6Class(
  "remotes",
  public = list(
    initialize = function(repo = default_repo())
      remotes_init(self, private, repo),
    add_remotes = function(specs)
      remotes_add_remotes(self, private, specs),
    add_remotes_from_config = function()
      remotes_add_remotes_from_config(self, private),
    remove_remotes = function(specs)
      remotes_remove_remotes(self, private, specs),
    resolve = function()
      remotes_resolve(self, private),
    get_resolution = function()
      remotes_get_resolution(self, private),
    diff_resolution = function()
      remotes_diff_resolution(self, private),
    download = function()
      remotes_download(self, private),
    get_download_status = function()
      remotes_get_download_status(self, private),
    get_config = function()
      private$cfg,
    commit = function()
      remotes_commit(self, private)
  ),

  private = list(
    dirty = FALSE,
    remotes = list(),
    repo = NULL,
    cfg  = NULL,
    resolution = NULL,
    downloads = NULL,

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
      file.path(private$repo, "_download_cache"),

    download_cran = function(resolution)
      remotes__download_cran(self, private, resolution),
    download_github = function(resolution)
      remotes__download_github(self, private, resolution),

    save_resolution = function()
      remotes__save_resolution(self, private),
    load_resolution = function()
      remotes__load_resolution(self, private)
  )
)

remotes_init <- function(self, private, repo) {
  private$repo <- normalizePath(repo)
  private$cfg <- repo_config(private$repo)
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

remotes_add_remotes_from_config <- function(self, private) {
  packages <- private$cfg$get("packages")
  self$add_remotes(packages)
}

remotes__update_cache <- function(self, private, type) {
  private$resolution$cache[[type]] <-
    if (type == "cran") {
      private$update_cache_cran()
    } else if (type == "github") {
      ## Nothing to do
    }
}

#' @importFrom cranlike create_empty_PACKAGES package_versions
#'   add_PACKAGES remove_PACKAGES

remotes_commit <- function(self, private) {

  if (is.null(private$resolution)) {
    stop("Need to resolve the remotes first")
  }

  if (is.null(private$downloads)) {
    stop("Need to download the remotes first")
  }

  dl <- self$get_download_status()
  if (any(dl$download_status == "Failed")) {
    stop("Cannot commit with failed downloads")
  }

  platforms <- private$cfg$get("config:platforms")
  rversions <- private$cfg$get("config:r-versions")
  dirs <- get_all_package_dirs(platforms, rversions)

  ## First write out the config file. If the commit is interrupted,
  ## then we can always use this to re-commit or re-download, or even
  ## re-resolve.
  private$cfg$save()

  cache <- private$get_download_cache_dir()
  repo <- private$repo

  ## We also save result of the resolution
  private$save_resolution()

  for (i in seq_len(nrow(dirs))) {
    dir <- dirs$contriburl[i]
    message("Updating ", dirs$contriburl[i])
    mkdirp(repodir <- file.path(repo, dir))
    create_empty_PACKAGES(repodir)

    ## Downloaded files
    dld <- dl$target[dl$repodir == dir]

    ## Existing files
    ex <- file.path(
      dir,
      package_versions(repodir, xcolumns = "File")$File
    )

    ## New files, some of these might be in the directory already,
    ## if we have a broken repo. We assume that these are good files,
    ## because we checked their etag (in theory the etag might belong to
    ## another file, though).
    new <- setdiff(dld, ex)
    new <- new[file.exists(file.path(cache, new))]
    if (length(new)) {
      message("Adding ", length(new), " new packages")
      file.rename(file.path(cache, new), file.path(repo, new))
    }
    add_PACKAGES(basename(new), repodir)

    ## Deleted files
    del <- setdiff(ex, dld)
    if (length(del)) {
      message("Deleting ", length(del), " packages")
      remove_PACKAGES(basename(del), repodir)
    }

    ## Updated files, these might have been cached
    upd <- intersect(ex, dld)
    upd <- upd[file.exists(file.path(cache, upd))]
    if (length(upd)) {
      message("Updating ", length(upd), " packages")
      remove_PACKAGES(basename(upd), repodir)
      file.rename(file.path(cache, upd), file.path(repo, upd))
      add_PACKAGES(basename(upd), repodir)
    }
  }

  ## The files are not there any more, so we remove the metadata
  private$downloads <- NULL

  invisible(self)
}

remotes__save_resolution <- function(self, private) {
  resolution_cache <- file.path(private$repo, "resolution.rds")
  resolution <- private$resolution
  remotes <- private$remotes
  save(resolution, remotes, file = resolution_cache)
}

remotes__load_resolution <- function(self, private) {
  resolution_cache <- file.path(private$repo, "resolution.rds")
  load(resolution_cache, envir = env <- new.env(parent = emptyenv()))
  private$remotes <- env$remotes
  private$resolution <- env$resolution
  invisible(self)
}
