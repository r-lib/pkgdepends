
remotes__add_fast_refs <- function(self, private, refs, remotes, direct) {

  cache <- private$resolution$cache
  cache$crandata <- cache$crandata %||%
    update_crandata_cache(private$config, private$progress_bar)

  if (!is.null(remotes)) {
    refs <- vcapply(remotes, "[[", "ref")
    pkgs <- vcapply(remotes, "[[", "package")
  } else {
    pkgs <- refs
  }

  if (direct) {
    private$resolution$fast$direct_refs <-
      c(private$resolution$fast$direct_refs, refs)
  } else {
    private$resolution$fast$indirect_refs <-
      c(private$resolution$fast$indirect_refs, refs)
  }
}

remotes__fast_resolve <- function(self, private) {
  self; private
  cache <- private$resolution$cache
  cache$crandata$then(~ remotes__fast_resolve1(self, private))
}

remotes__fast_resolve1 <- function(self, private) {
  self; private
  cache <- private$resolution$cache
  config <- private$config

  refs <- unique(c(private$resolution$fast$direct_refs,
                   private$resolution$fast$indirect_refs))
  direct <- refs %in% private$resolution$fast$direct_refs
  no_refs <- length(refs)
  if (!no_refs) return()

  ## We want to vectorize this as much as possible. So we work with a
  ## data frame throughout
  t_remote <- parse_remotes(refs)
  ref_df <- tibble(
    ref = refs,
    direct = direct,
    status = "OK",
    remote = t_remote,
    type = vcapply(t_remote, "[[", "type"),
    package = vcapply(t_remote, "[[", "package")
  )

  ## if bioc types, start getting bioc metadata
  if ("bioc" %in% ref_df$type) {
    cache$biocdata <- cache$biocdata %||%
      update_biocdata_cache(config, private$progress_bar)
  }

  cran_ref_df <- ref_df[ref_df$type != "bioc", ]
  bioc_ref_df <- ref_df[ref_df$type == "bioc", ]

  dirs <- cache$crandata$get_value()$`_dirs`

  files <- lapply(seq_len(nrow(dirs)), function(i) {
    dir <- dirs[i, ]
    data <- cache$crandata$get_value()[[dir$contriburl]]
    make_fast_cran_resolution(self, private, cran_ref_df, dir, data)
  })

  files <- fix_null_deps(do.call(rbind, files))

  ## Look at the failed once, to see if we should try BioC
  failed_pkgs <- unique(files$package[files$status == "FAILED"])
  retry_pkgs <- if (length(failed_pkgs)) {
    ## They might be in cran_ref_df, if they are, check if `cran` or
    ## `standard` refs, because we don't try `cran` refs again.
    cran_ref_df_failed <- which(cran_ref_df$package %in% failed_pkgs)
    cran_ref_df_retry <- if (length(cran_ref_df_failed)) {
      cran_ref_df_failed_pkg <- cran_ref_df$package[cran_ref_df_failed]
      cran_ref_df_failed_type <- cran_ref_df$type[cran_ref_df_failed]
      cran_ref_df_failed_pkg[cran_ref_df_failed_type != "cran"]
    }

    ## We retry if a package was *not* specified as cran::, or if the
    ## package was not in cran_ref_df at all
    failed_pkgs_indirect <- setdiff(failed_pkgs, cran_ref_df$package)
    c(cran_ref_df_retry, failed_pkgs_indirect)
  }

  if (length(retry_pkgs)) {
    t_ref <- retry_pkgs
    t_remote <- parse_remotes(t_ref)
    retry_df <- tibble(
      ref = t_ref,
      direct = retry_pkgs %in% cran_ref_df$package,
      status = "OK",
      remote = t_remote,
      type = vcapply(t_remote, "[[", "type"),
      package = vcapply(t_remote, "[[", "package")
    )
    bioc_ref_df <- rbind(bioc_ref_df, retry_df)
  }

  if (nrow(bioc_ref_df)) {
    cache$biocdata <- cache$biocdata %||%
      update_biocdata_cache(config, private$progress_bar)
    cache$biocdata$
      then(~ remotes__fast_resolve_bioc(self, private, bioc_ref_df))$
      then(function(bioc_files) {
        fast_finish_resolve(private, ref_df, files, bioc_files)
      })

  } else {
    fast_finish_resolve(private, ref_df, files, NULL)
  }
}

fix_null_deps <- function(files, data) {

  null_deps <- vlapply(files$deps, is.null)
  if (num_null_deps <- sum(null_deps)) {
    files$deps[null_deps] <- replicate(
      num_null_deps,
      make_null_deps(),
      simplify = FALSE
    )
  }
  files
}

fast_finish_resolve <- function(private, ref_df, cran_files, bioc_files) {

  ## We need to clean `files` a bit, because for a BioC package with a
  ## standard ref, it contains the CRAN resolution errors.
  files <- if (!is.null(bioc_files)) {
    ok_bioc_pkgs <- bioc_files$package[bioc_files$status == "OK"]
    no_cran_pkgs <- cran_files$package[cran_files$status != "OK"]
    if (length(remove <- intersect(ok_bioc_pkgs, no_cran_pkgs))) {
      types <- lapply(remove, function(x) ref_df$type[ref_df$package == x])
      no_cran <- vlapply(types, function(x) all(x != "cran"))
      cran_files <- cran_files[cran_files$status == "OK" |
                               ! cran_files$package %in% remove[no_cran], ]
    }
    rbind(cran_files, bioc_files)

  } else {
    cran_files
  }

  ## If the packages are in the library, then we need to schedule
  ## and installed:: ref
  pkgs <- unique(files$package)
  if (!is.null(private$library) && file.exists(private$library)) {
    ext_pkgs <- file.exists(file.path(private$library, pkgs))
    ins_pkgs <- pkgs[ext_pkgs]
    if (length(ins_pkgs)) {
      lib <- normalizePath(private$library, winslash = "/")
      ref <- paste0("installed::", lib, "/", ins_pkgs)
      ins_refs <- setdiff(ref, names(private$resolution$packages))
      ins_rems <- parse_remotes(ins_refs)
      for (rem in ins_rems) private$resolve_ref(rem, pool = FALSE)
    }
  }

  add_fast_resolution_result <- function(ref, type, package, remote,
                                         myfiles) {
    msg_type <- switch(type, cran = "CRAN", bioc = "BioC", "CRAN/BioC")
    if (length(myfiles)) {
      xfiles <- apply(files[myfiles,], 1, as.list)
      for (i in seq_along(xfiles)) {
        if (xfiles[[i]]$mode == "cran") {
          xfiles[[i]]$metadata <- list(
            RemoteOriginalRef = ref,
            RemoteType = "cran",
            RemoteRepos =
              paste0(deparse(xfiles[[i]]$mirror[[1]]), collapse = ""),
            RemotePkgType =
              if (xfiles[[i]]$platform == "source") "source" else "binary"
          )
        } else if (xfiles[[i]]$mode == "bioc") {
          xfiles[[i]]$metadata <- list(
            RemoteOriginalRef = ref,
            RemoteType = "bioc",
            RemoteRepos =
              paste0(deparse(xfiles[[i]]$mirror[[1]]), collapse = ""),
            RemotePkgType =
              if (xfiles[[i]]$platform == "source") "source" else "binary",
            RemoteRelease = xfiles[[i]]$bioc_version
          )
        }

        if (xfiles[[i]]$status == "FAILED") {
          xfiles[[i]]$error <- make_error(
            paste0("Can't find ", msg_type, " package ", package),
            class = "remotes_resolution_error"
          )
        }
      }
      res <- list(
        files = xfiles,
        direct = package %in% ref_df$package[ref_df$direct],
        remote = remote,
        status = all_ok(xfiles)
      )

    } else {
      res <- list(
        files = list(list(
          source = character(), target = NA_character_, platform = "*",
          rversion = "*", dir = NA_character_, package = package,
          version = NA_character_, deps = NA,
          needs_compilation = NA_character_, status = "FAILED",
          metadata = list(),
          error = make_error(
            paste0("Can't find ", msg_type, " package ", package, ", version ", version),
            class = "remotes_resolution_error"
          )
        )),
        direct = package %in% ref_df$package[ref_df$direct],
        remote = remote,
        status = "FAILED"
      )
    }
    class(res) <-
      c(paste0("remote_resolution_", type), "remote_resolution")
    private$resolution$packages[[ref]] <- res
  }

  ## Now fill up private$resolution$packages from files
  ## First the refs in the original list
  for (i in seq_len(nrow(ref_df))) {
    ref <- ref_df$ref[i]
    type <- ref_df$type[i]
    package <- ref_df$package[i]
    remote <- ref_df$remote[[i]]
    myfiles <- which(files$package == package)
    add_fast_resolution_result(ref, type, package, remote, myfiles)
  }
  if (nn <- sum(ref_df$direct)) private$progress_bar$update("count", nn)
  if (nn <- sum(!ref_df$direct)) private$progress_bar$update("xcount", nn)

  ## Next the dependencies
  dep_pkgs <- na.omit(setdiff(files$package, ref_df$package))
  if (nn <- length(dep_pkgs)) private$progress_bar$update("xtotal", nn)
  dep_rems <- fast_standard_remotes(dep_pkgs)
  for (i in seq_along(dep_pkgs)) {
    ref <- dep_pkgs[i]
    type <- "standard"
    package <- ref
    remote <- dep_rems[[i]]
    myfiles <- which(files$package == package)
    add_fast_resolution_result(ref, type, package, remote, myfiles)
  }
  if (nn) private$progress_bar$update("xcount", nn)
}

fast_standard_remotes <- function(refs) {
  res <- replicate(
    length(refs),
    structure(
      list(package = "", atleast = "", version = "", ref = "",
           type = "standard"),
      class = c("remote_ref_standard", "remote_ref", "list")
    ),
    simplify = FALSE
  )
  for (i in seq_along(refs)) {
    res[[i]]$package <- res[[i]]$ref <- refs[i]
  }
  res
}

make_fast_cran_resolution <- function(self, private, df, dir, data) {
  make_fast_resolution(self, private, df, dir, data, mode = "cran")
}

fast_get_path <- function(data, dir, ext) {
  if ("File" %in% names(data)) {
    if ("Path" %in% names(data)) {
      ifelse(!is.na(data$File),
             paste0(dir, data$File),
      ifelse(!is.na(data$Path),
             paste0(dir, "/", data$Path, "/", data$Package, "_",
                    data$Version, ext),
             paste0(dir, "/", data$Package, "_", data$Version, ext)
             )
      )

    } else {
      ifelse(!is.na(data$File),
             paste0(dir, data$File),
             paste0(dir, data$Package, "_", data$Version, ext))
    }

  } else if ("Path" %in% names(data)) {
    ifelse(!is.na(data$Path),
           paste0(dir, "/", data$Path, "/", data$Package, "_",
                  data$Version, ext),
           paste0(dir, "/", data$Package, "_", data$Version, ext)
           )
  } else {
    paste0(dir, "/", data$Package, "_", data$Version, ext)
  }
}

fast_get_source <- function(mode, mirror, platform, path, package,
                            version) {
  url <- paste0(mirror, "/", path)

  if (platform != "source" || mode != "cran") {
    as.list(url)
  } else {
    url2 <- paste0(mirror, "/src/contrib/Archive/", package, "/",
                   package, "_", version, ".tar.gz")
    mapply(c, url, url2, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
}

remotes__fast_resolve_bioc <- function(self, private, bioc_ref_df) {
  ## At this point, all refs are parsed, and marked as direct/indirect
  ## We also know that the bioc metadata is available, and the CRAN
  ## metadata as well
  self; private

  cache <- private$resolution$cache
  crandata <- cache$crandata$get_value()
  config <- private$config
  dirs <- crandata$`_dirs`

  biocdata <- cache$biocdata$get_value()
  repos <- biocdata$`_repos`

  files <- lapply(seq_len(nrow(dirs)), function(i) {
    dir <- dirs[i, ]
    lapply(names(repos), function(rversion) {
      cdata <- crandata[[dir$contriburl]]
      data <- biocdata[[dir$contriburl]][[rversion]]
      myrepos <- repos[[rversion]]
      make_fast_bioc_resolution(self, private, bioc_ref_df, dir, cdata,
                                data, myrepos)
    })
  })

  files <- unlist(files, recursive = FALSE)
  files <- fix_null_deps(do.call(rbind, files))

  files
}

make_fast_bioc_resolution <- function(self, private, ref_df, dir, cran_data,
                                      data, repos) {
  mirror <- private$config$`cran-mirror`
  mdata <- merge_bioc_data(cran_data, data, repos, mirror)
  make_fast_resolution(self, private, ref_df, dir, mdata, mode = "bioc")
}

bioc_repo_col_name <- function() "X-RPKG-BioCRepo"

#' Merge the BioC package metadata from the various BioC repos
#'
#' This will simplify dependency lookup, and we can use the same algorithm
#' as for CRAN metadata (or any CRAN-like repo, maybe?).
#'
#' We add an `X-RPKG-BioCRepo` column to the `pkgs` data frames, merge them,
#' and update the `idx` columns in the `deps` data frames, before merging
#' them as well.
#'
#' @param cran_data The cran metadata data frame.
#' @param data Named list of repo metadata. Each entry has two tibbles in
#'   a list: `pkgs` and `deps`.
#' @param repos List with entries: `repos`, the actual repository URLs, in
#'   a named character vector, and `version`, the R version that belongs
#'   to these BioC repos.
#' @param mirror CRAN mirror URL.
#' @return A list with two entries: `pkgs` and `deps`, the merged package
#'   tibble and dependency tibble.
#'
#' @keywords internal
#' @importFrom tibble as.tibble

merge_bioc_data <- function(cran_data, data, repos, mirror) {
  ## Some repos are empty, e.g. binary repos for data packages
  ## It is easiest to drop them here
  empty_repos <- vlapply(data, function(x) nrow(x$pkgs) == 0)
  data <- data[! empty_repos]
  repos$repos <- repos$repos[!empty_repos]

  ## Add cran_data to the repo data list
  data <- c(list(CRAN = cran_data), data)
  repos$repos <- c(CRAN = unname(mirror), repos$repos)

  ## Add repo information to `pkgs`
  col_name <- bioc_repo_col_name()
  for (i in seq_along(data)) {
    data[[i]]$pkgs[[col_name]] <- repos$repos[[i]]
  }

  ## Merge the pkgs data, we need to potentially expand the columns first,
  ## because although rbind considers column names, it requires the same
  ## column names for all input data frames
  bioc_version <- repos$version
  cols <- unique(unlist(lapply(data, function(x) colnames(x$pkgs))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]$pkgs))
    if (length(miss_cols)) {
      na_df <- as.tibble(structure(
        replicate(length(miss_cols), NA_character_, simplify = FALSE),
        names = miss_cols))
      data[[i]]$pkgs <- as.tibble(cbind(data[[i]]$pkgs, na_df,
                                        bioc_version = bioc_version))
    } else {
      data[[i]]$pkgs <- as.tibble(cbind(data[[i]]$pkgs,
                                        bioc_version = bioc_version))
    }
  }

  ## Now merge pkgs
  pkgs <- do.call(rbind, lapply(data, "[[", "pkgs"))

  ## Shift the idx in deps
  pkgs_rows <- viapply(data, function(x) nrow(x$pkgs), USE.NAMES = FALSE)
  shifts <- c(0, pkgs_rows)
  for (i in seq_along(data)) {
    data[[i]]$deps$idx <- data[[i]]$deps$idx + shifts[i]
  }

  ## Merge deps as well
  deps <- do.call(rbind, lapply(data, "[[", "deps"))

  list(pkgs = pkgs, deps = deps)
}

make_fast_resolution <- function(self, private, df, dir, data, mode) {
  ## We know that i_pkgs and d_pkgs are disjunct, made sure of that
  ## upstream.

  config <- private$config
  deps <- data$deps
  meta <- private$resolution$metadata
  d_cols <- meta$dependencies
  i_cols <- meta$indirect_dependencies
  d_pkgs <- df$package[df$direct]
  i_pkgs <- df$package[!df$direct]
  mirror <- if (mode == "cran") config$`cran-mirror`
  ext <- get_cran_extension(dir$platform)

  ## Do one round first, because we might have direct refs.
  ## After this round, we'll only have indirect refs
  pkgs <- df$package
  new <- setdiff(c(
    deps$package[deps$upstream %in% d_pkgs & deps$type %in% d_cols],
    deps$package[deps$upstream %in% i_pkgs & deps$type %in% i_cols]
  ), pkgs)

  while (length(new)) {
    pkgs <- unique(c(pkgs, new))
    new <- setdiff(
      deps$package[deps$upstream %in% new & deps$type %in% i_cols],
      pkgs)
  }

  pkgs <- setdiff(pkgs, c("R", base_packages()))

  done <- intersect(data$pkgs$Package, pkgs)
  err <- setdiff(pkgs, done)

  d_done <- intersect(done, d_pkgs)
  i_done <- setdiff(done, d_done)

  d_deps <- deps[deps$upstream %in% d_done & deps$type %in% d_cols, ]
  i_deps <- deps[deps$upstream %in% i_done & deps$type %in% i_cols, ]

  scols <- c("ref", "type", "package", "op", "version")
  all_deps <- c(split(d_deps[, scols], d_deps$idx),
                split(i_deps[, scols], i_deps$idx))

  ## One difficulty here is that each ref (i.e. each package) might
  ## appear multiple times in the package metadata table. So the length
  ## of our 'files' table is not the same as the number of refs. We
  ## temporarily keep 'idx' to refer to the instances of the packages.
  idx <- which(data$pkgs$Package %in% done)
  done_data <- data$pkgs[idx, ]
  col_name <- bioc_repo_col_name()
  real_mirror <- mirror %||% done_data[[col_name]]

  t_package <- done_data$Package
  t_target <- fast_get_path(done_data, dir$contriburl, ext)

  if ("NeedsCompilation" %in% colnames(done_data)) {
    needs_comp <- done_data$NeedsCompilation
    needs_comp <- ifelse(is.na(needs_comp), "no", needs_comp)
  } else {
    needs_comp <- rep("no", length(idx))
  }

  files <- tibble(
    idx = idx,
    package = t_package,
    version = done_data$Version,
    needs_compilation = needs_comp[seq_along(idx)],
    mirror = real_mirror,
    target = t_target,
    source = fast_get_source(mode, real_mirror, dir$platform, t_target,
                             t_package, version),
    platform = dir$platform,
    rversion = dir$rversion,
    dir = dir$contriburl,
    deps = unname(all_deps[as.character(idx)]),
    mode = mode,
    bioc_version =
      done_data[["bioc_version"]] %||% rep(NA_character_, length(idx)),
    status = "OK"
  )

  if (num_err <- length(err)) {
    efiles <- tibble(
      idx = rep(NA_integer_, num_err),
      package = err,
      version = rep(NA_character_, num_err),
      needs_compilation = rep(NA_character_, num_err),
      mirror = NA_character_,
      target = rep(NA_character_, num_err),
      source = replicate(num_err, character(), simplify = FALSE),
      platform = dir$platform,
      rversion = dir$rversion,
      dir = dir$contriburl,
      deps = replicate(num_err, NULL, simplify = FALSE),
      mode = mode,
      bioc_version = done_data[["bioc_version"]][1] %||% NA_character_,
      status = "FAILED"
    )
    files <- rbind(files, efiles)
  }

  files
}
