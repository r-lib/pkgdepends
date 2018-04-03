
cranlike_metadata_cache <- R6Class(
  "cranlike_metadata_cache",

  public = list(
    initialize = function(primary_path = user_cache_dir("R-pkg"),
                          replica_path = tempfile(),
                          platforms = default_platforms(),
                          r_version = current_r_version(), bioc = TRUE,
                          cran_mirror = default_cran_mirror(),
                          update_after = as.difftime(7, units = "days"))
      cmc_init(self, private,  primary_path, replica_path, platforms,
               r_version, bioc, cran_mirror, update_after),

    deps = function(packages, recursive = TRUE, update = "auto")
      synchronise(self$deps(packages, recursive, update)),
    async_deps = function(packages, recursive = TRUE, update = "auto")
      cmc_async_deps(self, private, packages, recursive, update),

    update = function()
      synchronise(update(self, private)),
    async_update = function()
      cmc_async_update(self, private),

    needs_update = function()
      cmc_needs_update(self, private)
  ),

  private = list(
    data = NULL,
    
    primary_path = NULL,
    replica_path = NULL,
    platforms = NULL,
    r_version = NULL,
    bioc = NULL,
    cran_mirror = NULL,
    update_after = NULL,
    dirs = NULL,

    get_cache_dir = function(which = c("primary", "replica"))
      cmc__get_cache_dir(self, private, match.arg(which)),

    maybe_update = function(update = "auto")
      cmc__maybe_update(self, private, update)
  )
)

#' @importFrom filelock lock unlock

cmc_init <- function(self, private, primary_path, replica_path, platforms,
                    r_version, bioc, cran_mirror, update_after) {

  private$primary_path <- primary_path
  private$replica_path <- replica_path
  private$platforms <- platforms
  private$r_version <- r_version
  private$bioc <- bioc
  private$cran_mirror <- cran_mirror
  private$update_after <- update_after

  private$dirs <- get_all_package_dirs(platforms, r_version)

  pri <- private$get_cache_dir("primary")
  rep <- private$get_cache_dir("replica")

  mkdirp(rep$meta)

  l <- lock(pri$lock, exclusive = FALSE, timeout = 10000)
  if (is.null(l)) {
    stop("Cannot lock primary metadata directory at ", pri$root)
  }
  on.exit(unlock(l))
  file.copy(pri$meta, dirname(rep$meta), recursive = TRUE)
  invisible(self)
}

cmc_async_deps <- function(self, private, packages, recursive, update) {
  assert_that(
    is_character(packages),
    is_flag(recursive),
    is_flag(update) || update == "auto")
    
  private$maybe_update(update)$
    then(~ extract_deps(., packages, recursive))
}

cmc_async_update <- function(self, private) {
  TODO
}

cmc_needs_update <- function(self, private) {
  TODO
}

cmc__get_cache_dir <- function(self, private, which) {
  root <- private[[paste0(which, "_path")]]
  list(
    root = root,
    meta = file.path(root, "_metadata"),
    lock = file.path(root, "_metadata.lock"))
}

cmc__maybe_update <- function(self, private, update) {
  if (update || needs_update()) self$update() else private$data
}

extract_deps <- function(pkgs, packages, recursive) {
  TODO
}


#' @importFrom tools md5sum file_ext

CRANMetadataCache <- R6Class(
  "CRANMetadataCache",

  public = list(
    initialize = function() {
      private$data <- new.env(parent = emptyenv())
      invisible(self)
    },

    get = function(path) {
      path <- normalizePath(path)
      md5 <- md5sum(path)
      if (! is.null(res <- private$data[[md5]])) {
        res
      } else {
        private$insert_file(path, md5)
      }
    }
  ),

  private = list(
    data = NULL,

    insert_file = function(path, md5) {
      ext <- file_ext(path)
      if (ext == "gz") {
        private$insert_file_gz(path, md5)
      } else if (ext == "rds") {
        private$insert_file_rds(path, md5)
      }
    },

    insert_file_gz = function(path, md5) {
      pkgs <- format_packages_gz(read.dcf.gz(path))
      private$data[[md5]] <- pkgs
      pkgs
    },

    insert_file_rds = function(path, md5) {
      obj <- format_archive_rds(readRDS(path))
      private$data[[md5]] <- obj
      obj
    }
  )
)

#' @importFrom tibble as_tibble

format_packages_gz <- function(pkgs) {
  pkgs <- as_tibble(pkgs)
  list(pkgs = pkgs, deps = fast_parse_deps(pkgs))
}

format_archive_rds <- function(ards) {

  files <- sub("^[^/]+/", "", unlist(lapply(ards, rownames)))

  tibble(
    package = rep(names(ards), viapply(ards, nrow)),
    file = files,
    version = sub("^[^_]+_([-\\.0-9]+)\\.tar\\.gz$", "\\1", files),
    size = unlist(unname(lapply(ards, "[[", "size")))
  )
}

update_crandata_cache <- function(config, progress_bar) {
  type_cran_update_cache(
    rootdir   = config$metadata_cache_dir,
    platforms = config$platforms,
    rversions = config$`r-versions`,
    mirror    = config$`cran-mirror`,
    progress_bar
  )
}

update_biocdata_cache <- function(config, progress_bar) {
  type_bioc_update_cache(
    rootdir   = config$metadata_cache_dir,
    platforms = config$platforms,
    rversions = config$`r-versions`,
    progress_bar
  )
}
