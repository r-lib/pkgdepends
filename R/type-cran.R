
## ------------------------------------------------------------------------
## API

#' @importFrom rematch2 re_match
#' @importFrom stats na.omit
#' @importFrom desc desc_get_deps
#' @importFrom tibble as_tibble tibble
#' @export

parse_remote.remote_specs_cran <- function(specs, config, ...) {

  parsed_specs <- re_match(specs, standard_rx("cran"))

  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "cran"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

#' @export

resolve_remote.remote_ref_cran <- function(remote, config, ..., cache) {
  force(remote)
  cache$crandata <- cache$crandata %||% update_crandata_cache(config)

  cache$crandata$then(function(cacheresult) {
    type_cran_resolve_from_cache(remote, config, cacheresult)
  })
}

#' @export

download_remote.remote_resolution_cran <- function(resolution, config,
                                                   ..., cache) {
  ref <- resolution$remote$ref

  async_map(resolution$files, function(files) {
    get_package_from(cache$package_cache, files$source,
                     config$cache_dir, files$target)
  })
}

#' @export

satisfies_remote.remote_resolution_cran <- function(resolution, candidate,
                                                    config, ...) {

  ## 1. candidate must be a cran, standard or installed ref
  if (! inherits(candidate, "remote_resolution_cran") &&
      ! inherits(candidate, "remote_resolution_standard") &&
      ! inherits(candidate, "remote_resolution_installed")) {
    return(FALSE)
  }

  ## 2. installed refs must be from CRAN
  if (inherits(candidate, "remote_resolution_installed")) {
    dsc <- candidate$remote$description
    if (! identical(dsc$get("Repository")[[1]], "CRAN")) return(FALSE)
  }

  ## 3. package names must match
  if (resolution$remote$package != candidate$remote$package) return(FALSE)

  ## 4. version requirements must be satisfied. Otherwise good.
  if (resolution$remote$version == "") return(TRUE)

  version_satisfies(
    candidate$files[[1]]$version,
    resolution$remote$atleast,
    resolution$remote$version
  )
}

## ----------------------------------------------------------------------
## Internal functions

type_cran_update_cache <- function(rootdir, platforms, rversions, mirror) {
  dirs <- get_all_package_dirs(platforms, rversions)

  defs <- lapply_with_names(dirs$contriburl, function(dir) {
    cache_file  <- file.path(dir, "_cache", "PACKAGES.gz")
    target_file <- file.path(rootdir, cache_file)
    source_url  <- paste0(mirror, "/", dir, "/PACKAGES.gz")
    cache_etag  <- file.path(dir, "_cache", "etags.yaml")
    etag_file   <- file.path(rootdir, cache_etag)
    mkdirp(dirname(target_file))
    download_if_newer(source_url, target_file, etag_file)$
      then(function(resp) {
        if (resp$status_code == 200) {
          update_metadata_cache(rootdir, c(cache_file, cache_etag))
        }
      })$
      then(function() {
        cran_metadata_cache$get(target_file)
      })
  })

  archive <- local({
    cache_file <- file.path("src/contrib", "_cache", "archive.rds")
    target_rds <- file.path(rootdir, cache_file)
    source_url <- paste0(mirror, "/src/contrib/Meta/archive.rds")
    mkdirp(dirname(target_rds))
    cache_etag <- paste0(cache_file, ".etag")
    etag_file  <- paste0(target_rds, ".etag")
    download_if_newer(source_url, target_rds, etag_file)$
      then(function(resp) {
        if (resp$status_code == 200) {
          update_metadata_cache(rootdir, c(cache_file, cache_etag))
        }
      })$
      then(function() {
        cran_metadata_cache$get(target_rds)
      })
  })

  cran_cache <- when_all(
    `_dirs` = dirs,
    `_archive` = archive,
    .list = defs
  )

  cran_cache
}

type_cran_resolve_from_cache <- function(remote, config, crancache) {
  if (remote$version == "current" || remote$version == "") {
    type_cran_resolve_from_cache_current(remote, config, crancache)
  } else {
    type_cran_resolve_from_cache_general(remote, config, crancache)
  }
}

type_cran_resolve_from_cache_current <- function(remote, config,
                                                 crancache) {
  files <- type_cran_resolve_from_cache_current_files(remote, config,
                                                      crancache)

  files$then(function(files) {
    status <- if (all(vcapply(files, "[[", "status") == "OK")) {
      "OK"
    } else {
      "FAILED"
    }
    structure(
      list(files = files, remote = remote, status = status),
      class = c("remote_resolution_cran", "remote_resolution")
    )
  })
}

type_cran_resolve_from_cache_current_files <- function(remote, config,
                                                       crancache) {

  platforms    <- config$platforms
  rversions    <- config$`r-versions`
  mirror       <- config$`cran-mirror`
  dependencies <- config$dependencies
  dirs         <- crancache$`_dirs`

  files <- lapply(seq_len(nrow(dirs)), function(i) {
    dir <- dirs[i, ]
    type_cran_make_resolution(
      remote,
      dir$platform,
      dir$rversion,
      data = crancache[[dir$contriburl]],
      dir = dir$contriburl,
      mirror = mirror,
      dependencies = dependencies
    )
  })

  async_constant(files)
}

type_cran_resolve_from_cache_general <- function(remote, config,
                                                 crancache) {

  dependencies <- config$dependencies

  vers <- type_cran_fix_cran_version(
    remote$package, remote$version, remote$atleast,
    packages = crancache$`src/contrib`,
    archive = crancache$`_archive`
  )

  files <- async_map(
    vers,
    function(v) {
      if (v == "current") {
        rem2 <- remote
        rem2$version <- ""
        rem2$atleast <- ""
        type_cran_resolve_from_cache_current_files(rem2, config, crancache)
      } else {
        type_cran_resolve_from_cache_version_files(remote, v, config,
                                                   crancache)
      }
    }
  )

  files$then(function(files) {

    ## This is a list of lists
    files <- unlist(files, recursive = FALSE, use.names = FALSE)

    status <- if (all(vcapply(files, "[[", "status") == "OK")) {
      "OK"
    } else {
      "FAILED"
    }

    structure(
      list(files = files, remote = remote, status = status),
      class = c("remote_resolution_cran", "remote_resolution")
    )
  })
}

type_cran_resolve_from_cache_version_files <- function(remote, version,
                                                       config, crancache) {

  package <- remote$package
  mirror  <- config$`cran-mirror`
  dirs    <- crancache$`_dirs`
  dir     <- dirs$contriburl[match("source", dirs$platform)]
  archive <- crancache$`_archive`
  package_path <- archive$file[archive$package == package &
                                 archive$version == version]
  dependencies <- config$dependencies

  ## To get the dependencies, we need to download the package, and
  ## parse DESCRIPTION
  source <- type_cran_make_cran_archive_url(mirror, package, version)
  target_file <- file.path(config$cache_dir, dir, package_path)
  mkdirp(target_dir <- dirname(target_file))
  etag_file <- file.path(target_dir, "_cache", basename(target_file))

  deps <- type_cran_get_package_deps_url(
    source, target_file, dependencies, last = TRUE, etag_file = etag_file)$
      then(function(deps) {
        list(list(
          source = type_cran_make_cran_archive_url(mirror, package, version),
          target = file.path(dir, package_path),
          platform = "source",
          rversion = "*",
          dir = dir,
          package = package,
          version = version,
          deps = deps,
          status = "OK"
        ))
      })
}

type_cran_make_resolution <- function(remote, platform, rversion, data,
                                      dir, mirror, dependencies) {
  ref <- remote$ref
  package <- remote$package
  version <- remote$version

  result <- list(
    source = character(), target = NA_character_, platform = platform,
    rversion = rversion, dir = dir, package = package,
    version = NA_character_, deps = NA, status = "OK"
  )

  wh <- if (version == "") {
    wh <- which(data[ , "Package"] == package)
  } else {
    wh <- which(data[ , "Package"] == package &
                  data[, "Version"] == version)
  }
  if (! length(wh)) {
    result$status <- "FAILED"
    return(result)
  }

  if (length(wh) > 1) warning("Non-unique resolve: ", sQuote(ref))
  wh <- wh[1]
  version <- unname(data[wh, "Version"])
  result$version <- version
  ext <- get_cran_extension(platform)

  path <- if ("File" %in% colnames(data) &&
              !is.na(file_loc <- data[wh, "File"])) {
    paste0(dir, "/", file_loc)
  } else {
    paste0(dir, "/", package, "_", version, ext)
  }

  url <- paste0(mirror, "/", path)

  ## If this is a source package, then it might be in Archive by the time
  ## we download it
  if (platform == "source") {
    url <- c(
      url,
      type_cran_make_cran_archive_url(mirror, package, version))
  }

  result$source <- unname(url)
  result$target <- path

  result$deps <- get_cran_deps(result$package, result$version,
                               data, dependencies)

  result$metadata <- c(
    RemoteOriginalRef = ref,
    RemoteType = "cran",
    RemoteRepos = mirror[[1]],
    RemotePkgType = if (platform == "source") "source" else "binary"
  )

  result
}

type_cran_make_cran_archive_url <- function(mirror, package, version) {
  paste0(mirror, "/src/contrib/Archive/", package, "/",
         package, "_", version, ".tar.gz")
}

type_cran_fix_cran_version <- function(package, version, ge, packages,
                                       archive) {

  current <- packages[packages[, "Package"] == package, "Version"]
  oldvers <- archive$version[archive$package == package]

  res <- if (version == "last") {
    if (length(current)) "current" else max(package_version(oldvers))

  } else if (ge == "") {
    if (version %in% current) {
      "current"
    } else if (version %in% oldvers) {
      version
    } else {
      stop("Cannot find package version ", version)
    }

  } else {
    c(oldvers[package_version(oldvers) >= version],
      if (length(current)) "current")
  }

  as.character(res)
}

type_cran_get_package_deps_url <- function(url, target, dependencies,
                                           last = FALSE,
                                           etag_file = NULL) {
  force(url) ; force(target) ; force(dependencies) ; force(last)
  force(etag_file)
  download_if_newer(url, target, etag_file)$
    then(function() desc_get_deps(file = target))$
    then(function(deps) deps_from_desc(deps, dependencies, last))
}
