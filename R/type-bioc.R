
## ------------------------------------------------------------------------
## API

#' @importFrom glue glue_data
#' @export

parse_remote.remote_specs_bioc <- function(specs, config, ...) {

  ## BioC is the same as CRAN, except for cran:: -> bioc::
  parsed_specs <- re_match(specs, standard_rx("bioc"))
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "bioc"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

#' @export

resolve_remote.remote_ref_bioc <- function(remote, config, cache,
                                           dependencies, ...) {
  force(remote); force(dependencies)
  cache$biocdata <- cache$biocdata %||% update_biocdata_cache(config)

  cache$biocdata$then(function(cacheresult) {
    type_bioc_resolve_from_cache(remote, config, cacheresult, dependencies)
  })
}

#' @export

download_remote.remote_resolution_bioc <- function(resolution, config,
                                                   ..., cache, progress_bar) {
  async_map(get_files(resolution), function(files) {
    meta <- files[c("platform", "package", "version", "rversion")]
    get_package_from(cache$package_cache, files$source,
                     config$cache_dir, files$target, metadata = meta,
                     progress_bar = progress_bar)
  })
}

#' @export

satisfies_remote.remote_resolution_bioc <- function(resolution, candidate,
                                                    config, ...) {

  ## 1. candidate must be a bioc, standard or installed ref
  if (! inherits(candidate, "remote_resolution_bioc") &&
      ! inherits(candidate, "remote_resolution_standard") &&
      ! inherits(candidate, "remote_resolution_installed")) {
    return(FALSE)
  }

  ## 2. installed refs must be from bioc
  if (inherits(candidate, "remote_resolution_installed")) {
    dsc <- get_remote(candidate)$description
    if (is.na(dsc$get("biocViews"))) return(FALSE)
  }

  ## 3. package names must match
  if (get_remote(resolution)$package != get_remote(candidate)$package) {
    return(FALSE)
  }

  ## 4. version requirements must be satisfied. Otherwise good.
  if (get_remote(resolution)$version == "") return(TRUE)

  version_satisfies(
    get_files(candidate)[[1]]$version,
    get_remote(resolution)$atleast,
    get_remote(resolution)$version
  )
}

## ----------------------------------------------------------------------
## Internal functions

type_bioc_matching_bioc_version <- function(r_version) {
  if (r_version >= "3.5") {
    "3.6"
  } else if (r_version >= "3.4") {
    "3.5"
  } else if (r_version >= "3.3.0") {
    "3.4"
  } else if (r_version >= "3.2") {
    "3.2"
  } else if (r_version >= "3.1.1") {
    "3.0"
  } else if (r_version == "3.1.0") {
    "2.14"
  } else if (r_version >= "2.15" && r_version <= "2.16") {
    "2.11"
  } else {
    stop("Cannot get matching BioConductor version for ", r_version)
  }
}

type_bioc_get_bioc_repos <- function(r_version) {
  bv <- type_bioc_matching_bioc_version(r_version)
  tmpl <- c(
    BioCsoft  = "https://bioconductor.org/packages/{bv}/bioc",
    BioCann   = "https://bioconductor.org/packages/{bv}/data/annotation",
    BioCexp   = "https://bioconductor.org/packages/{bv}/data/experiment",
    BioCextra = "https://bioconductor.org/packages/{bv}/extra"
  )
  list(
    repos = vcapply(tmpl, glue_data, .x = list(bv = bv)),
    version = bv
  )
}

#' @importFrom utils URLencode

type_bioc_update_cache <- function(rootdir, platforms, rversions) {
  rootdir; platforms; rversions

  dirs <- get_all_package_dirs(platforms, rversions)

  bioc_repos <- lapply_with_names(rversions, type_bioc_get_bioc_repos)

  defs <- lapply_with_names(dirs$contriburl, function(dir) {
    dir
    names(rversions) <- rversions
    async_map(rversions, function(rversion) {
      repos <- bioc_repos[[rversion]]$repos
      async_map(repos, function(repo) {
        urepo <- URLencode(repo, reserved=TRUE)
        cache_file <- file.path(dir, "_cache", "bioc", urepo, "PACKAGES.gz")
        target_file <- file.path(rootdir, cache_file)
        source_url <- paste0(repo, "/", dir, "/", "PACKAGES.gz")
        cache_etag <- file.path(dir, "_cache", "bioc", urepo, "etags.yaml")
        etag_file <- file.path(rootdir, cache_etag)
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
    })
  })

  biocdata <- when_all(
    `_dirs` = dirs,
    `_repos` = bioc_repos,
    .list = defs
  )

  biocdata
}

type_bioc_resolve_from_cache <- function(remote, config, bioccache,
                                         dependencies) {

  files <- type_bioc_resolve_from_cache_files(remote, config, bioccache,
                                              dependencies)

  files$then(function(files) {
    status <- if (all(vcapply(files, "[[", "status") == "OK")) {
      "OK"
    } else {
      "FAILED"
    }
    structure(
      list(files = files, remote = remote, status = status),
      class = c("remote_resolution_bioc", "remote_resolution")
    )
  })
}

type_bioc_resolve_from_cache_files <- function(remote, config, bioccache,
                                               dependencies) {
  platforms    <- config$platforms
  rversions    <- config$`r-versions`
  dirs         <- bioccache$`_dirs`
  repos        <- bioccache$`_repos`

  files <- lapply(seq_len(nrow(dirs)), function(i) {
    dir <- dirs[i, ]
    lapply(names(repos), function(rversion) {
      unlist(type_bioc_make_bioc_resolution(
        remote,
        dir$platform,
        dir$rversion,
        bioc_version = repos[[rversion]]$version,
        data = bioccache[[dir$contriburl]][[rversion]],
        repos = repos[[rversion]]$repos,
        dir = dir$contriburl,
        dependencies = dependencies
      ), recursive = FALSE)
    })
  })

  files <- unlist(files, recursive = FALSE, use.names = FALSE)

  async_constant(files)
}

type_bioc_make_bioc_resolution <- function(remote, platform, rversion,
                                           bioc_version, data, repos,
                                           dir, dependencies) {

  ref <- remote$ref
  package <- remote$package
  version <- remote$version

  result <- list(
    source = character(), target = NA_character_, platform = platform,
    rversion = rversion, dir = dir, package = package,
    version = NA_character_, deps = NA, status = "OK")

  ## Some binary repos are empty, e.g. experiment and annotation repos
  keep <- vlapply(data, function(d) nrow(d$pkgs) != 0)
  data <- data[keep]
  repos <- repos[keep]

  ## Which BioC repo do we need?
  which_repo <- vlapply(data, function(d) package %in% d$pkgs$Package)
  if (sum(which_repo) == 0) {
    result$status <- "FAILED"
    result$error <- make_error(
      paste0("Can't find BioConductor package ", package),
      class = "remotes_resolution_error"
    )
    return(list(result))
  } else if (sum(which_repo) > 1) {
    warning("Package '", package, "' in multiple repositories, using first")
  }
  data <- data[which_repo][[1]]
  repos <- repos[which_repo][[1]]

  dependencies <- intersect(dependencies, colnames(data$pkgs))

  wh <- if (version == "") {
    wh <- which(data$pkgs$Package == package)
  } else {
    wh <- which(data$pkgs$Package == package &
                  data$pkgs$Version == version)
  }
  if (! length(wh)) {
    result$status <- "FAILED"
    result$error <- make_error(
      paste0("Can't find BioConductor package ", package),
      class = "remotes_resolution_error"
    )
    return(list(result))
  }

  ext <- get_cran_extension(platform)

  result <- replicate(length(wh), result, simplify = FALSE)
  for (i in 1:length(wh)) {
    whi <- wh[i]
    version <- data$pkgs$Version[[wh]]
    result[[i]]$version <- version

    path <- if ("File" %in% colnames(data$pkgs) &&
                !is.na(file_loc <- data$pkgs$File[[wh]])) {
      paste0(dir, "/", file_loc)
    } else if ("Path" %in% colnames(data$pkgs) &&
               !is.na(file_path <- data$pkgs$path[[whi]])) {
      paste0(dir, "/", file_path, "/", package, "_", version, ext)
    } else {
      paste0(dir, "/", package, "_", version, ext)
    }

    url <- paste0(repos, "/", path)

    result[[i]]$source <- unname(url)
    result[[i]]$target <- path

    result[[i]]$deps <- fast_select_deps(data$deps, whi, dependencies)

    result[[i]]$metadata <- c(
      RemoteOriginalRef = ref,
      RemoteType = "bioc",
      RemoteRepos = paste0(deparse(repos), collapse = ""),
      RemotePkgType = if (platform == "source") "source" else "binary",
      RemoteRelease = bioc_version
    )
  }

  result
}
