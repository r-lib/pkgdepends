
#' @importFrom curl parse_headers_list

remotes__update_cache_cran <- function(self, private) {
  remotes_i_update_cache_cran(
    repo      = private$repo,
    platforms = private$cfg$get("config:platforms"),
    rversions = private$cfg$get("config:r-versions"),
    mirror    = private$cfg$get("config:cran-mirror")
  )
}

remotes_i_update_cache_cran <- function(repo, platforms, rversions,
                                        mirror) {

  dirs <- get_all_package_dirs(platforms, rversions)

  defs <- lapply_with_names(dirs$contriburl, function(dir) {
    target_rds  <- file.path(repo, dir, "_cache", "PACKAGES.rds")
    target_file <- file.path(repo, dir, "_cache", "PACKAGES.gz")
    source_url  <- paste0(mirror, "/", dir, "/PACKAGES.gz")
    etag_file   <- file.path(repo, dir, "_cache", "etags.yaml")
    mkdirp(dirname(target_file))
    download_if_newer(source_url, target_file, etag_file)$
      then(function(value) {
        pkgs <- read.dcf.gz(target_file)
        saveRDS(pkgs, target_rds)
        pkgs
      })
  })

  archive <- local({
    target_rds <- file.path(repo, "src/contrib", "_cache", "archive.rds")
    source_url <- paste0(mirror, "/src/contrib/Meta/archive.rds")
    mkdirp(dirname(target_rds))
    etag_file <- paste0(target_rds, ".etag")
    download_if_newer(source_url, target_rds, etag_file)$
      then(function() {
        format_archive_rds(readRDS(target_rds))
      })
  })

  when_all(`_dirs` = dirs, `_archive` = archive, .list = defs)
}

remotes__resolve_ref_cran <- function(self, private, rem) {
  message("Scheduling resolution of ", rem$ref)

  ## This is conditional on the CRAN being ready
  private$resolution$cache$cran$then(function(cache) {
    if (rem$version == "current" || rem$version == "") {
      private$resolve_ref_cran_current(rem)
    } else {
      private$resolve_ref_cran_general(rem)
    }
  })
}

remotes__resolve_ref_cran_current_files <- function(self, private, rem) {

  platforms    <- private$cfg$get("config:platforms")
  rversions    <- private$cfg$get("config:r-versions")
  mirror       <- private$cfg$get("config:cran-mirror")
  dependencies <- private$cfg$get("config:dependencies")
  cache        <- await(private$resolution$cache$cran)
  dirs         <- cache$`_dirs`

  files <- lapply(seq_len(nrow(dirs)), function(i) {
    dir <- dirs[i, ]
    result <- make_cran_resolution(
      rem,
      dir$platform,
      dir$rversion,
      data = cache[[dir$contriburl]],
      dir = dir$contriburl,
      mirror = mirror,
      dependencies = dependencies
    )
    dep_rems <- parse_remotes(result$deps)
    lapply(dep_rems, function(r) private$resolve_ref(r))
    result
  })

  async_constant(files)
}

remotes__resolve_ref_cran_current <- function(self, private, rem) {

  files <- remotes__resolve_ref_cran_current_files(self, private, rem)

  files$then(function(files) {
    status <- if (all(vcapply(files, "[[", "status") == "OK")) {
      "OK"
    } else {
      "FAILED"
    }
    list(files = files, remote = rem, status = status)
  })
}

remotes__resolve_ref_cran_general <- function(self, private, rem) {
  cache <- await(private$resolution$cache$cran)
  dependencies <- private$cfg$get("config:dependencies")

  vers <- fix_cran_version(
    rem$package, rem$version, rem$atleast,
    packages = cache$`src/contrib`,
    archive = cache$`_archive`
  )

  files <- async_map(
    vers,
    function(v) {
      if (v == "current") {
        rem2 <- rem
        rem2$version <- ""
        rem2$atleast <- ""
        remotes__resolve_ref_cran_current_files(self, private, rem2)
      } else {
        private$resolve_ref_cran_version_files(rem$package, version = v,
                                               dependencies)
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

    list(files = files, remote = rem, status = status)
  })
}

remotes__resolve_ref_cran_version_files <- function(self, private, package,
                                                    version, dependencies) {
  repo    <- private$repo
  mirror  <- private$cfg$get("config:cran-mirror")
  cache   <- await(private$resolution$cache$cran)
  dirs    <- cache$`_dirs`
  dir     <- dirs$contriburl[match("source", dirs$platform)]
  archive <- cache$`_archive`
  package_path <- archive$file[archive$package == package &
                                 archive$version == version]

  ## To get the dependencies, we need to download the package, and
  ## parse DESCRIPTION
  source <- make_cran_archive_url(mirror, package, version)
  target_file <- file.path(repo, dir, package_path)
  cached_target <-
    file.path(private$get_download_cache_dir(), dir, package_path)
  mkdirp(target_dir <- dirname(target_file))
  mkdirp(dirname(cached_target))
  etag_file <- file.path(target_dir, "_cache", basename(target_file))

  deps <- get_package_deps_url(source, cached_target, etag_file)$
    then(function(deps) {
      clean_package_deps(deps, dependencies, last = TRUE)
    })$
    then(function(deps) {
      dep_rems <- parse_remotes(deps)
      lapply(dep_rems, function(r) private$resolve_ref(r))
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

clean_package_deps <- function(deps, dependencies, last = FALSE) {
  pkgs <- deps[deps$type %in% dependencies, ]$package
  pkgs <- setdiff(pkgs, c("R", base_packages()))
  if (last && length(pkgs)) paste0(pkgs, "@last") else pkgs
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

  result$source <- url
  result$target <- path

  result$deps <- get_cran_deps(remote$ref, result$package, result$version,
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

#' @importFrom stats na.omit

get_cran_deps <- function(ref, package, version, data, dependencies) {

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

  deps <- data[wh, dependencies]
  res <- unname(unlist(parse_deps(na.omit(deps))))

  ## TODO: Bioc? Additional repositories?
  res
}

#' Work out all package directories
#'
#' Based on the platforms and R versions that we support.
#'
#' @param platforms Can be a subset of `source`, `windows`, `macos`. The
#'   platforms to download.
#' @param rversions The R versions to download.
#'
#' @keywords internal

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

  res <- as.data.frame(do.call(rbind, res), stringsAsFactors = FALSE)
  colnames(res) <- c("platform", "rversion", "contriburl")
  res$prefix <- paste0(
    "/",
    ifelse(res$rversion == "*", "*", paste0("R-", res$rversion)),
    "/", res$platform, "/"
  )

  res
}

#' Package file extension for a platform
#'
#' @param platform Platform.
#' @return File extension.
#'
#' @keywords internal

get_cran_extension <- function(platform) {
  switch(
    platform,
    "source" = ".tar.gz",
    "macos" = ".tgz",
    "windows" = ".zip",
    stop("Unknown platform: ", sQuote(platform))
  )
}

#' Parse dependency specifications, vectorized
#'
#' @param deps Vector of dependency specifications.
#' @return Character vector of dependent packages. Base packages are
#'   omitted.
#'
#' @keywords internal

parse_deps <- function(deps) {
  deps <- lapply(strsplit(deps, ","), str_trim)
  deps <- lapply(deps, function(x) lapply(strsplit(x, "\\("), str_trim))
  deps <- lapply(
    deps,
    function(x) lapply(x, sub, pattern = "\\)$", replacement = "")
  )
  deps <- lapply(deps, function(x) vapply(x, "[", "", 1))
  lapply(deps, setdiff, y = c("R", base_packages()))
}

#' @importFrom desc desc_get_deps

get_package_deps_url <- function(url, target, etag_file = NULL) {
  force(url) ; force(target) ; force(etag_file)
  download_if_newer(url, target, etag_file)$
    then(function() desc_get_deps(file = target))
}
