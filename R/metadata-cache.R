
cmc__data <- new.env(parent = emptyenv())

cranlike_metadata_cache <- R6Class(
  "cranlike_metadata_cache",

  public = list(
    initialize = function(primary_path = user_cache_dir("R-pkg"),
                          replica_path = tempfile(),
                          platforms = default_platforms(),
                          r_version = current_r_version(), bioc = TRUE,
                          cran_mirror = default_cran_mirror(),
                          repos = getOption("repos"),
                          update_after = as.difftime(7, units = "days"))
      cmc_init(self, private,  primary_path, replica_path, platforms,
               r_version, bioc, cran_mirror, repos, update_after),

    deps = function(packages, dependencies = NA, recursive = TRUE)
      synchronise(self$async_deps(packages, dependencies, recursive)),
    async_deps = function(packages, dependencies = NA, recursive = TRUE)
      cmc_async_deps(self, private, packages, dependencies, recursive),

    revdeps = function(packages, dependencies = NA, recursive = TRUE)
      synchronise(self$async_revdeps(packages, dependencies, recursive)),
    async_revdeps = function(packages, dependencies = NA, recursive = TRUE)
      cmc_async_revdeps(self, private, packages, dependencies, recursive),

    list = function(packages = NULL)
      synchronise(self$async_list(packages)),
    async_list = function(packages = NULL)
      cmc_async_list(self, private, packages),

    update = function()
      synchronise(self$async_update()),
    async_update = function()
      cmc_async_update(self, private)
  ),

  private = list(
    get_cache_files = function(which = c("primary", "replica"))
      cmc__get_cache_files(self, private, match.arg(which)),

    async_ensure_cache = function(max_age)
      cmc__async_ensure_cache(self, private, max_age),

    get_current_data = function(max_age)
      cmc__get_current_data(self, private, max_age),
    get_memory_cache = function(max_age)
      cmc__get_memory_cache(self, private, max_age),
    load_replica_rds = function(max_age)
      cmc__load_replica_rds(self, private, max_age),
    load_primary_rds = function(max_age)
      cmc__load_primary_rds(self, private, max_age),
    load_primary_pkgs = function(max_age)
      cmc__load_primary_pkgs(self, private, max_age),

    update_replica_pkgs = function()
      cmc__update_replica_pkgs(self, private),
    update_replica_rds = function()
      cmc__update_replica_rds(self, private),
    update_primary = function(rds = TRUE, packages = TRUE)
      cmc__update_primary(self, private, rds, packages),
    update_memory_cache = function()
      cmc__update_memory_cache(self, private),

    data = NULL,
    data_time = NULL,

    update_deferred = NULL,

    primary_path = NULL,
    replica_path = NULL,
    platforms = NULL,
    r_version = NULL,
    bioc = NULL,
    repos = NULL,
    update_after = NULL,
    dirs = NULL,
    lock_timeout = 10000
  )
)

#' @importFrom filelock lock unlock

cmc_init <- function(self, private, primary_path, replica_path, platforms,
                     r_version, bioc, cran_mirror, repos, update_after) {

  "!!DEBUG Init metadata cache in '`replica_path`'"
  private$primary_path <- primary_path
  private$replica_path <- replica_path
  private$platforms <- platforms
  private$r_version <- get_minor_r_version(r_version)
  private$bioc <- bioc
  private$repos <- cmc__get_repos(repos, bioc, cran_mirror, r_version)
  private$update_after <- update_after
  private$dirs <- get_all_package_dirs(platforms, r_version)
  invisible(self)
}

cmc_async_deps <- async(function(self, private, packages, dependencies,
                                 recursive) {
  assert_that(
    is_character(packages),
    is_dependencies(dependencies),
    is_flag(recursive))

  "!!DEBUG Getting deps"
  private$async_ensure_cache(private$update_after)$
    then(~ extract_deps(., packages, dependencies, recursive))
})

cmc_async_revdeps <- async(function(self, private, packages, dependencies,
                                    recursive) {
  assert_that(
    is_character(packages),
    is_dependencies(dependencies),
    is_flag(recursive))

  "!!DEBUG Getting revdeps"
  private$async_ensure_cache(private$update_after)$
    then(~ extract_revdeps(., packages, dependencies, recursive))
})

cmc_async_list <- async(function(self, private, packages) {
  assert_that(is.null(packages) || is_character(packages))

  "!!DEBUG Listing packages"
  private$async_ensure_cache(private$update_after)$
    then(function(x) {
      if (is.null(packages)) x$pkgs else x$pkgs[x$pkgs$package %in% packages,]
    })
})

cmc_async_update <- function(self, private) {
  if (!is.null(private$update_deferred)) return(private$update_deferred)

  private$update_deferred <- private$update_replica_pkgs()$
    then(~ private$update_replica_rds())$
    then(~ private$update_primary())$
    then(~ private$data)$
    finally(function() private$update_deferred <- NULL)$
    share()
}

#' @importFrom digest digest

repo_encode <- function(repos) {
  paste0(
    vcapply(repos$name, URLencode, reserved = TRUE), "-",
    substr(vcapply(repos$url, digest), 1, 10)
  )
}

cmc__get_cache_files <- function(self, private, which) {
  root <- private[[paste0(which, "_path")]]

  repo_hash <- digest(list(repos = private$repos, dirs = private$dirs))

  str_platforms <- paste(private$platforms, collapse = "+")
  rds_file <- paste0("pkgs-", substr(repo_hash, 1, 10), ".rds")

  repo_enc <- rep(repo_encode(private$repos), each = nrow(private$dirs))
  pkgs_dirs <- rep(private$dirs$contriburl, nrow(private$repos))
  pkgs_files <- file.path(pkgs_dirs, "PACKAGES.gz")
  mirror <- rep(private$repos$url, each = nrow(private$dirs))
  type <- rep(private$repos$type, each = nrow(private$dirs))
  bioc_version <- rep(private$repos$bioc_version, each = nrow(private$dirs))

  list(
    root = root,
    meta = file.path(root, "_metadata"),
    lock = file.path(root, "_metadata.lock"),
    rds  = file.path(root, "_metadata", rds_file),
    pkgs = tibble::tibble(
      path = file.path(root, "_metadata", repo_enc, pkgs_files),
      etag = file.path(root, "_metadata", repo_enc, paste0(pkgs_files, ".etag")),
      basedir = pkgs_dirs,
      base = pkgs_files,
      mirror = mirror,
      url = paste0(mirror, "/", pkgs_files),
      platform = rep(private$dirs$platform, nrow(private$repos)),
      type = type,
      bioc_version = bioc_version
    )
  )
}

#' Load the cache, asynchronously, with as little work as possible
#'
#' 1. If it is already loaded, and fresh return it.
#' 2. Otherwise try the replica RDS.
#' 3. Otherwise try the primary RDS.
#' 4. Otherwise try the primary PACKAGES files.
#' 5. Otherwise update the replica PACKAGES files,
#'    the replica RDS, and then also the primary PACKAGES and RDS.
#'
#' @param self self
#' @param private private self
#' @param max_age Maximum age allowed to consider the data current.
#' @return Metadata.
#' @keywords internal

cmc__async_ensure_cache <- async(function(self, private, max_age) {
  max_age

  async_try_each(
    async(private$get_current_data)(max_age),
    async(private$get_memory_cache)(max_age),
    async(private$load_replica_rds)(max_age),
    async(private$load_primary_rds)(max_age),
    async(private$load_primary_pkgs)(max_age),
    self$async_update()

  )$catch(error = function(err) {
    err$message <- "Could not load or update metadata cache"
    stop(err)
  })
})

cmc__get_current_data <- function(self, private, max_age) {
  "!!DEBUG Get current data?"
  if (is.null(private$data)) stop("No data loaded")
  if (is.null(private$data_time) ||
      Sys.time() - private$data_time > max_age) {
    stop("Loaded data outdated")
  }
  "!!DEBUG Got current data!"

  private$data
}

cmc__get_memory_cache  <- function(self, private, max_age) {
  "!!DEBUG Get from memory cache?"
  rds <- private$get_cache_files("primary")$rds
  hit <- cmc__data[[rds]]
  if (is.null(hit)) stop("Not in the memory cache")
  if (is.null(hit$data_time) || Sys.time() - hit$data_time > max_age) {
    stop("Memory cache outdated")
  }
  private$data <- hit$data
  private$data_time <- hit$data_time

  private$data
}

#' Try to load the package metadata asynchronously, from the replica RDS
#'
#' If the replica has the RDS data, it is loaded and returned.
#' Otherwise throws an error.
#'
#' @param self Self.
#' @param private Private self.
#' @param max_age Maximum age allowed for the RDS file to be considered
#'   as current.
#' @return The metadata.
#' @keywords internal

cmc__load_replica_rds <- function(self, private, max_age) {
  "!!DEBUG Load replica RDS?"
  rds <- private$get_cache_files("replica")$rds
  if (!file.exists(rds)) stop("No replica RDS file in cache")

  time <- file_get_time(rds)
  if (Sys.time() - time > max_age) stop("Replica RDS cache file outdated")

  private$data <- readRDS(rds)
  private$data_time <- time
  "!!DEBUG Loaded replica RDS!"
  private$update_memory_cache()

  private$data
}

#' Load the metadata from the primary cache's RDS file
#'
#' If it exists and current, then the replica RDS is updated to it as well,
#' and the data is returned. Otherwise throws an error.
#'
#' @inheritParams cmc__load_replica_rds
#' @return Metadata.
#' @keywords internal

cmc__load_primary_rds <- function(self, private, max_age) {
  "!!DEBUG Load primary RDS?"
  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  l <- lock(pri_files$lock, exclusive = FALSE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to copy RDS")
  on.exit(unlock(l), add = TRUE)

  if (!file.exists(pri_files$rds)) stop("No primary RDS file in cache")
  time <- file_get_time(pri_files$rds)
  if (Sys.time() - time > max_age) stop("Primary RDS cache file outdated")

  file_copy_with_time(pri_files$rds, rep_files$rds)
  unlock(l)

  private$data <- readRDS(rep_files$rds)
  private$data_time <- time

  private$update_memory_cache()

  private$data
}

#' Load metadata from the primary cache's PACKAGES files
#'
#' If they are not available, or outdated, it throws an error.
#' Otherwise they are copied to the replica cache, and then used
#' to create the RDS file. The RDS file is then written back to the
#' primary cache and also loaded.
#'
#' @param self self
#' @param private private self
#' @param max_age Max age to consider the files current.
#' @return Metadata.
#' @keywords internal

cmc__load_primary_pkgs <- function(self, private, max_age) {
  "!!DEBUG Load replica PACKAGES*?"
  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  ## Lock
  l <- lock(pri_files$lock, exclusive = FALSE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to copy PACKAGES files")
  on.exit(unlock(l), add = TRUE)

  ## Check if PACKAGES exist and current
  if (!all(file.exists(pri_files$pkgs$path))) {
    stop("Some primary PACKAGES files don't exist")
  }
  time <- file_get_time(pri_files$pkgs$path)
  if (any(Sys.time() - time > max_age)) {
    stop("Some primary PACKAGES files are outdated")
  }

  ## Copy to replica, if we cannot copy the etags, that's ok
  file_copy_with_time(pri_files$pkgs$path, rep_files$pkgs$path)
  tryCatch(
    file_copy_with_time(pri_files$pkgs$etag, rep_files$pkgs$etag),
    error = function(e) e
  )
  unlock(l)

  ## Update RDS in replica, this also loads it
  private$update_replica_rds()

  ## Update primary, but not the PACKAGES
  private$update_primary(rds = TRUE, packages = FALSE)

  private$data
}

#' Update the PACKAGES files in the replica cache
#'
#' I.e. download them, if they have changed.
#'
#' @param self self
#' @param private private self
#' @keywords internal

cmc__update_replica_pkgs <- async(function(self, private) {
  "!!DEBUG Update replica PACKAGES"
  pkgs <- private$get_cache_files("replica")$pkgs

  dls <- lapply_rows(pkgs, function(pkg) {
    download_if_newer(pkg$url, pkg$path, pkg$etag)
  })

  when_all(.list = dls)
})

#' Update the replica RDS from the PACKAGES files
#'
#' Also loads it afterwards.
#'
#' @param self self
#' @param private private self
#' @keywords internal

cmc__update_replica_rds <- function(self, private) {
  "!!DEBUG Update replica RDS"
  rep_files <- private$get_cache_files("replica")

  data_list <- lapply_rows(
    rep_files$pkgs,
    function(r) {
      tryCatch(
        read_packages_file(r$path, mirror = r$mirror,
                           repodir = r$basedir, platform = r$platform,
                           rversion = private$r_version, type = r$type),
        ## TODO: warn?
        error = function(x) NULL
      )
    })

  data_list <- data_list[!vlapply(data_list, is.null)]

  private$data <- merge_packages_data(.list = data_list)
  saveRDS(private$data, file = rep_files$rds)
  private$data_time <- file_get_time(rep_files$rds)

  private$update_memory_cache()

  private$data
}

#' Update the primary cache from the replica cache
#'
#' @param self self
#' @param private private self
#' @param rds Whether to update the RDS file.
#' @param packages Wheren  to update the PACKAGES files (+ Etag files).
#' @return Nothing.
#'
#' @keywords internal

cmc__update_primary <- function(self, private, rds, packages) {

  "!!DEBUG Updata primary cache"
  if (!rds && !packages) return()

  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  mkdirp(dirname(pri_files$lock))
  l <- lock(pri_files$lock, exclusive = TRUE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to update primary cache")
  on.exit(unlock(l), add = TRUE)

  if (rds) {
    file_copy_with_time(rep_files$rds, pri_files$rds)
  }
  if (packages) {
    file_copy_with_time(rep_files$pkgs$path, pri_files$pkgs$path)
    file_copy_with_time(rep_files$pkgs$etag, pri_files$pkgs$etag)
  }
  unlock(l)

  invisible()
}

cmc__update_memory_cache <- function(self, private) {
  rds <- private$get_cache_files("primary")$rds
  cmc__data[[rds]] <- list(data = private$data, data_time = private$data_time)
}

extract_deps <- function(pkgs, packages, dependencies, recursive) {

  realdep <- interpret_dependencies(dependencies)
  dep <- tolower(realdep$direct)

  new <- packages
  repeat {
    new <- setdiff(
      pkgs$deps$package[pkgs$deps$upstream %in% new &
                        pkgs$deps$type %in% dep],
      packages)
    if (!length(new)) break
    packages <- c(packages, new)
    if (!recursive) break
    dep <- tolower(realdep$indirect)
  }

  packages <- setdiff(packages, "R")
  res <- pkgs$pkgs[pkgs$pkgs$package %in% packages, ]

  base <- intersect(packages, base_packages())
  attr(res, "base") <- base
  attr(res, "unknown") <- setdiff(packages, c(res$package, base))

  res
}

extract_revdeps <- function(pkgs, packages, dependencies, recursive) {

  realdep <- interpret_dependencies(dependencies)
  dep <- tolower(realdep$direct)

  new <- packages
  repeat {
    new <- setdiff(
      pkgs$deps$upstream[pkgs$deps$ref %in% new & pkgs$deps$type %in% dep],
      packages)
    if (!length(new)) break
    packages <- c(packages, new)
    if (!recursive) break
    dep <- tolower(realdep$indirect)
  }

  packages <- setdiff(packages, "R")
  res <- pkgs$pkgs[pkgs$pkgs$package %in% packages, ]

  base <- intersect(packages, base_packages())
  attr(res, "base") <- base
  attr(res, "unknown") <- setdiff(packages, c(res$package, base))

  res
}

cmc__get_repos <- function(repos, bioc, cran_mirror, r_version) {
  repos[["CRAN"]] <- cran_mirror
  res <- tibble(
    name = names(repos),
    url = unname(repos),
    type = ifelse(names(repos) == "CRAN", "cran", "cranlike"),
    bioc_version = NA_character_)

  if (bioc) {
    bioc_repos <- type_bioc_get_bioc_repos(r_version)
    bioc_version <- bioc_repos$version
    bioc_repos <- bioc_repos$repos

    miss <- setdiff(names(bioc_repos), res$name)
    bioc_res <- tibble(
      name = miss,
      url = unname(bioc_repos[miss]),
      type = "bioc",
      bioc_version = bioc_version
    )
    res <- rbind(res, bioc_res)
    res$type[res$name %in% names(bioc_repos)] <- "bioc"
    res$bioc_version[res$name %in% names(bioc_repos)] <- bioc_version
  }

  res
}

#' Query CRAN(like) package data
#'
#' It uses CRAN and BioConductor packages.
#'
#' `cran_list()` lists all packages.
#'
#' `cran_update()` updates all metadata. Note that metadata is automatically
#' updated if it is older than seven days.
#'
#' `cran_deps()` queries packages dependencies.
#'
#' `cran_revdeps()` queries reverse package dependencies.
#'
#' @param packages Packages to query.
#' @param dependencies Dependency types to query. See the `dependencies`
#'   parameter of [utils::install.packages()].
#' @param recursive Whether to query recursive dependencies.
#' @return A data frame (tibble) of the dependencies. For `cran_deps()`
#'   it includes the queried `packages` as well.
#'
#' @section Examples:
#' ```
#' cran_deps("dplyr")
#' cran_list(c("MASS", dplyr"))
#' cran_update()
#' ```
#'
#' @export

cran_deps <- function(packages, dependencies = NA, recursive = TRUE) {
  global_metadata_cache$deps(packages, dependencies, recursive)
}

#' @export
#' @rdname cran_deps

cran_revdeps <- function(packages, dependencies = NA, recursive = TRUE) {
  global_metadata_cache$revdeps(packages, dependencies, recursive)
}

#' @export
#' @rdname cran_deps

cran_update <- function() {
  invisible(global_metadata_cache$update()$pkgs)
}

#' @export
#' @rdname cran_deps

cran_list <- function(packages = NULL) {
  global_metadata_cache$list(packages)
}
