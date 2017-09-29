
## ------------------------------------------------------------------------
## API

#' @export
parse_remote.remote_specs_cran <- NULL
#' @export
resolve_remote.remote_ref_cran <- NULL
#' @export
download_remote.remote_resolution_cran <- NULL

#' @importFrom rematch2 re_match
#' @importFrom stats na.omit
#' @importFrom desc desc_get_deps
#' @importFrom tibble as_tibble tibble

local({

  ## ----------------------------------------------------------------------

  parse_remote.remote_specs_cran <<- function(specs, config, ...) {

    parsed_specs <- re_match(specs, cran_rx())

    parsed_specs$ref <- parsed_specs$.text
    cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
    parsed_specs <- parsed_specs[, cn]
    parsed_specs$type <- "cran"
    lapply(
      seq_len(nrow(parsed_specs)),
      function(i) as.list(parsed_specs[i,])
    )
  }

  ## ----------------------------------------------------------------------

  resolve_remote.remote_ref_cran <<- function(remote, config, ..., cache) {
    force(remote)
    if (is.null(cache$crandata)) {
      update_cache(
        cache,
        rootdir   = config$cache_dir,
        platforms = config$platforms,
        rversions = config$`r-versions`,
        mirror    = config$`cran-mirror`
      )
    }

    cache$crandata$then(function(cacheresult) {
      resolve_from_cache(remote, config, cacheresult)
    })
  }

  download_remote.remote_resolution_cran <<- function(resolution, config,
                                                      ..., cache) {
    ref <- resolution$remote$ref

    async_map(resolution$files, function(files) {
      urls <- files$source
      target_file <- file.path(config$cache_dir, files$target)
      mkdirp(target_dir <- dirname(target_file))
      etag_file <- file.path(target_dir, "_cache", basename(target_file))

      had_this <- first_existing_file(target_file, target_file)
      download_try_list(urls, target_file, etag_file)$
        then(function(status) {
          if (status == 304) {
            make_dl_status("Had", files, urls, target_file,
                           bytes = file.size(target_file))
          } else {
            make_dl_status("Got", files, urls, target_file,
                           bytes = file.size(target_file))
          }
        })$
        catch(function(err) {
          make_dl_status("Failed", files, urls, target_file,
                         error = err$error)
        })
    })
  }

  ## ----------------------------------------------------------------------
  ## Internal functions

  update_cache <- function(cache, rootdir, platforms, rversions, mirror) {
    dirs <- get_all_package_dirs(platforms, rversions)

    defs <- lapply_with_names(dirs$contriburl, function(dir) {
      target_rds  <- file.path(rootdir, dir, "_cache", "PACKAGES.rds")
      target_file <- file.path(rootdir, dir, "_cache", "PACKAGES.gz")
      source_url  <- paste0(mirror, "/", dir, "/PACKAGES.gz")
      etag_file   <- file.path(rootdir, dir, "_cache", "etags.yaml")
      mkdirp(dirname(target_file))
      download_if_newer(source_url, target_file, etag_file)$
      then(function(value) {
        pkgs <- read.dcf.gz(target_file)
        saveRDS(pkgs, target_rds)
        pkgs
      })
    })

    archive <- local({
      target_rds <- file.path(rootdir, "src/contrib", "_cache", "archive.rds")
      source_url <- paste0(mirror, "/src/contrib/Meta/archive.rds")
      mkdirp(dirname(target_rds))
      etag_file <- paste0(target_rds, ".etag")
      download_if_newer(source_url, target_rds, etag_file)$
      then(function() {
        format_archive_rds(readRDS(target_rds))
      })
    })

    cache$crandata <- when_all(
      `_dirs` = dirs,
      `_archive` = archive,
      .list = defs
    )
  }

  resolve_from_cache <- function(remote, config, crancache) {
    if (remote$version == "current" || remote$version == "") {
      resolve_from_cache_current(remote, config, crancache)
    } else {
      resolve_from_cache_general(remote, config, crancache)
    }
  }

  resolve_from_cache_current <- function(remote, config, crancache) {
    files <- resolve_from_cache_current_files(remote, config, crancache)

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

  resolve_from_cache_current_files <- function(remote, config, crancache) {

    platforms    <- config$platforms
    rversions    <- config$`r-versions`
    mirror       <- config$`cran-mirror`
    dependencies <- config$dependencies
    dirs         <- crancache$`_dirs`

    files <- lapply(seq_len(nrow(dirs)), function(i) {
      dir <- dirs[i, ]
      make_cran_resolution(
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

  resolve_from_cache_general <- function(remote, config, crancache) {

    dependencies <- config$dependencies

    vers <- fix_cran_version(
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
          resolve_from_cache_current_files(rem2, config, crancache)
        } else {
          resolve_from_cache_version_files(remote, v, config, crancache)
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

  resolve_from_cache_version_files <- function(remote, version, config,
                                               crancache) {

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
    source <- make_cran_archive_url(mirror, package, version)
    target_file <- file.path(config$cache_dir, dir, package_path)
    mkdirp(target_dir <- dirname(target_file))
    etag_file <- file.path(target_dir, "_cache", basename(target_file))

    deps <- get_package_deps_url(source, target_file, dependencies,
                                 last = TRUE, etag_file = etag_file)$
      then(function(deps) {
        list(list(
          source = make_cran_archive_url(mirror, package, version),
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

  get_all_package_dirs <- function(platforms, rversions) {
    minors <- unique(get_minor_r_version(rversions))
    res <- lapply(platforms, function(pl) {
      if (pl == "source") {
        cbind("source", "*", "src/contrib")

      } else if (pl == "windows") {
        cbind("windows", minors, paste0("bin/windows/contrib/", minors))

      } else if (pl == "macos") {
        res1 <- lapply(minors, function(v) {
          if (package_version(v) <= "2.15") {
            cbind("macos", v, paste0("bin/macosx/leopard/contrib/", v))
          } else if (package_version(v) == "3.0") {
            cbind("macos", v, paste0("bin/macosx/contrib/", v))
          } else if (package_version(v) <= "3.2") {
            cbind("macos", v, paste0(c("bin/macosx/contrib/",
                                       "bin/macosx/mavericks/contrib/"), v))
          } else if (package_version(v) == "3.3") {
            cbind("macos", v, paste0("bin/macosx/mavericks/contrib/", v))
          } else {
            cbind("macos", v, paste0("bin/macosx/el-capitan/contrib/", v))
          }
        })
        do.call(rbind, res1)
      }
    })

    res <- as_tibble(do.call(rbind, res), stringsAsFactors = FALSE)
    colnames(res) <- c("platform", "rversion", "contriburl")
    res$prefix <- paste0(
      "/",
      ifelse(res$rversion == "*", "*", paste0("R-", res$rversion)),
    "/", res$platform, "/"
    )

    res
  }

  make_cran_resolution <- function(remote, platform, rversion, data, dir,
                                   mirror, dependencies) {
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
      url <- c(url, make_cran_archive_url(mirror, package, version))
    }

    result$source <- unname(url)
    result$target <- path

    result$deps <- get_cran_deps(result$package, result$version,
                                 data, dependencies)

    result
  }

  make_cran_archive_url <- function(mirror, package, version) {
    paste0(mirror, "/src/contrib/Archive/", package, "/",
           package, "_", version, ".tar.gz")
  }

  fix_cran_version <- function(package, version, ge, packages, archive) {

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

  get_cran_deps <- function(package, version, data, dependencies) {

    ## Some dependency types might not be present here
    dependencies <- intersect(dependencies, colnames(data))

    wh <- if (version == "") {
      wh <- which(data[ , "Package"] == package)
    } else {
      wh <- which(data[ , "Package"] == package &
                    data[, "Version"] == version)
    }
    wh <- wh[1]
    version <- data[wh, "Version"]

    deps <- na.omit(data[wh, dependencies])
    res <- do.call(rbind, parse_deps(deps, names(deps)))
    res$ref <- res$package
    res <- res[, c("ref", setdiff(names(res), "ref"))]

    ## TODO: Bioc? Additional repositories?
    res
  }

  get_cran_extension <- function(platform) {
    switch(
      platform,
      "source" = ".tar.gz",
      "macos" = ".tgz",
      "windows" = ".zip",
      stop("Unknown platform: ", sQuote(platform))
    )
  }

  get_package_deps_url <- function(url, target, dependencies, last = FALSE,
                                   etag_file = NULL) {
    force(url) ; force(target) ; force(dependencies) ; force(last)
    force(etag_file)
    download_if_newer(url, target, etag_file)$
      then(function() desc_get_deps(file = target))$
      then(function(deps) deps_from_desc(deps, dependencies, last))
  }
})
