
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

resolve_remote.remote_ref_bioc <- function(remote, config, ..., cache) {
  force(remote)
  cache$biocdata <- cache$biocdata %||% update_biocdata_cache(config)

  cache$biocdata$then(function(cacheresult) {
    type_bioc_resolve_from_cache(remote, config, cacheresult)
  })
}

#' @export

download_remote.remote_resolution_bioc <- function(resolution, config,
                                                   ..., cache) {
  ref <- resolution$remote$ref

  async_map(resolution$files, function(files) {
    get_package_from(cache$package_cache, files$source,
                     config$cache_dir, files$target)
  })
}

#' @export

satisfies_remote.remote_resolution_bioc <-
  function(resolution, candidate, config, ...) {
    ## TODO
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
  vcapply(tmpl, glue_data, .x = list(bv = bv))
}

type_bioc_update_cache <- function(rootdir, platforms, rversions) {
  rootdir; platforms; rversions

  dirs <- get_all_package_dirs(platforms, rversions)

  bioc_repos <- lapply_with_names(rversions, type_bioc_get_bioc_repos)

  defs <- lapply_with_names(dirs$contriburl, function(dir) {
    dir
    names(rversions) <- rversions
    async_map(rversions, function(rversion) {
      repos <- bioc_repos[[rversion]]
      async_map(repos, function(repo) {
        target_file <- file.path(rootdir, dir, "_cache", "bioc", repo,
                                 "PACKAGES.gz")
        source_url <- paste0(repo, "/", dir, "/", "PACKAGES.gz")
        etag_file <- file.path(rootdir, dir, "_cache", "bioc", repo,
                               "etags.yaml")
        mkdirp(dirname(target_file))
        download_if_newer(source_url, target_file, etag_file)$
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

  ## TODO: this might copy partial files?
  biocdata$then(update_metadata_cache_dir(rootdir))

  biocdata
}

type_bioc_resolve_from_cache <- function(remote, config, bioccache) {
  files <- type_bioc_resolve_from_cache_files(remote, config, bioccache)

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

type_bioc_resolve_from_cache_files <- function(remote, config, bioccache) {
  platforms    <- config$platforms
  rversions    <- config$`r-versions`
  dependencies <- config$dependencies
  dirs         <- bioccache$`_dirs`
  repos        <- bioccache$`_repos`

  files <- lapply(seq_len(nrow(dirs)), function(i) {
    dir <- dirs[i, ]
    lapply(names(repos), function(rversion) {
      type_bioc_make_bioc_resolution(
        remote,
        dir$platform,
        dir$rversion,
        data = bioccache[[dir$contriburl]][[rversion]],
        repos = repos[[rversion]],
        dir = dir$contriburl,
        dependencies = dependencies
      )
    })
  })

  files <- unlist(files, recursive = FALSE, use.names = FALSE)

  async_constant(files)
}

type_bioc_make_bioc_resolution <- function(remote, platform, rversion, data,
                                           repos, dir, dependencies) {
  ref <- remote$ref
  package <- remote$package
  version <- remote$version

  result <- list(
    source = character(), target = NA_character_, platform = platform,
    rversion = rversion, dir = dir, package = package,
    version = NA_character_, deps = NA, status = "OK")

  ## Some binary repos are empty, e.g. experiment and annotation repos
  keep <- vlapply(data, function(d) length(d) != 0)
  data <- data[keep]
  repos <- repos[keep]

  ## Which BioC repo do we need?
  which_repo <- vlapply(data, function(d) package %in% d[, "Package"])
  if (sum(which_repo) == 0) {
    result$status <- "FAILED"
    return(result)
  } else if (sum(which_repo) > 1) {
    warning("Package '", package, "' in multiple repositories")
  }
  data <- data[which_repo][[1]]
  repos <- repos[which_repo][[1]]

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

  url <- paste0(repos, "/", path)

  result$source <- unname(url)
  result$target <- path

  result$deps <- get_cran_deps(result$package, result$version, data,
                               dependencies)
  result
}
