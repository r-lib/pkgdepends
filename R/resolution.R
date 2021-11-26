
#' Dependency resolution
#'
#' Collect information about dependencies of R packages, recursively.
#'
#' [`pkg_deps`], [`pkg_download_proposal`] and [`pkg_installation_proposal`]
#' all resolve their dependencies recursively, to obtain information about
#' all packages needed for the specified [package references][pkg_refs].
#'
#' ## CRAN and Bioconductor packages
#'
#' Resolution currently start by downloading the CRAN and Bioconductor
#' metadata, if it is out of date. For CRAN, we also download additional
#' metadata, that includes file sizes, SHA hashes, system requirements,
#' and "built" (for binary packages) and "packaged" time stamps. The extra
#' meta information is updated daily currently, so for some packages it
#' might be incorrect or missing.
#'
#' ## GitHub packages
#'
#' For GitHub packages, we query their download URL to be able to
#' download the package later, and also download their `DESCRIPTION`
#' file, to learn about their dependencies.
#'
#' ## Local packages
#'
#' From local package files we extract the `DESCRIPTION` file, to learn
#' about their dependencies.
#'
#' ## The `remotes` field in `DESCRIPTION`
#'
#' We support the non-standard `Remotes` field in the package `DESCRIPTION`
#' file. This field may contain a list of package references for any of the
#' dependencies that are specified in one of the `Depends`, `Includes`,
#' `Suggests` or `Enhances` fields. The syntax is a comma separated list of
#' [package references][pkg_refs].
#'
#' ## The result
#'
#' The result of the resolution is a data frame (tibble) with lots of
#' information about the packages and their dependencies. The columns that
#' are not documented here may be removed or changed, because they are
#' either used internally or experimental.
#'
#' * `built`: The `Built` field from the `DESCRIPTION` file of binary
#'   packages, for which this information is available.
#' * `cache_status`: Whether the package file is in the package cache.
#'   It is `NA` for `installed::` package refs.
#' * `dep_types`: Character vector of dependency types that were
#'   considered for this package. (This is a list column.)
#' * `deps`: Dependencies of the package, in a data frame (tibble). See
#'   'Package dependency tables' below.
#' * `direct`: Whether this package (ref, really) was directly specified,
#'   or added as a dependency.
#' * `error`: This is a list column that contains error objects for the
#'   refs that pkgdepends failed to resolve.
#' * `filesize`: The file size in bytes, or `NA` if this information is
#'   not available.
#' * `license`: License of the package, or `NA` if not available.
#' * `md5sum`: MD5 checksum of the package file, if available, or `NA` if
#'   not.
#' * `metadata`: A named character vector. These fields will be (should be)
#'   added to the installed `DESCRIPTION` file of the package.
#' * `mirror`: URL of the CRAN(-like) mirror site where the metadata was
#'   obtained from. It is NA for non-CRAN-like sources, e.g. local files,
#'   installed packages, GitHub, etc.
#' * `needscompilation`: Whether the package needs compilation.
#' * `package`: Package name.
#' * `priority`: This is `"base"` for base packages, `"recommended"` for
#'    recommended packages, and `NA` otherwise.
#' * `ref`: Package reference.
#' * `remote`: The parsed `remote_ref` objects, see [parse_pkg_refs()].
#'   This is a list column.
#' * `repodir`: The directory where this package should be in a CRAN-like
#'   repository.
#' * `sha256`: SHA256 hash of the package file, if available, otherwise
#'   `NA`.
#' * `sources`: URLs where this package can be downloaded from. This is a
#'    zero length vector for `installed::` refs.
#' * `status`: Status of the dependency resolution, `"OK"` or `"FAILED"`.
#' * `target`: Path where this package should saved in a CRAN-repository.
#' * `type`: Ref type.
#' * `version`: Package version.
#'
#' ## Package dependency tables
#'
#' A package dependency table has five columns currently:
#'
#' * `ref`: The package ref of the dependency.
#' * `type`: The dependency type, in all lowercase. I.e. `imports`,
#'   `suggests`, etc.
#' * `op`: Operator for version requirements, e.g. `>=`.
#' * `version`: Version number, for version requirements.
#'
#' ## Resolution failures
#'
#' The resolution process does not stop on error. Instead, failed
#' resolutions return and error object in the `error` column of the result
#' data frame.
#'
#' @name pkg_resolution
#' @aliases pkg_resolution_result
NULL

#' @importFrom prettyunits pretty_dt

pkgplan_resolve <- function(self, private) {
  "!DEBUG pkgplan_resolve (sync)"
  synchronise(self$async_resolve())
}

pkgplan_async_resolve <- function(self, private) {
  "!DEBUG pkgplan_resolve (async)"
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL
  private$solution <- NULL

  private$dirty <- TRUE
  private$resolution <- new_resolution(
    config = private$config, cache = private$cache,
    library = private$config$get("library"),
    remote_types = private$remote_types
  )

  private$resolution$push(direct = TRUE, .list = private$remotes)

  private$resolution$when_complete()$
    then(function(x) {
      private$dirty <- FALSE
      x
    })
}

pkgplan_get_resolution <- function(self, private) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  private$resolution$result
}

pkgplan__subset_resolution <- function(self, private, which) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  res <- private$resolution$result[which, ]
  attr(res, "metadata")  <- attr(private$resolution$result, "metadata")
  res
}

new_resolution <- function(config, cache, library = NULL,
                           remote_types = NULL) {
  resolution$new(config, cache, library, remote_types)
}

resolution <- R6::R6Class(
  "resolution",
  public = list(
    result = NULL,
    initialize = function(config, cache, library = NULL,
                          remote_types = NULL)
      res_init(self, private, config, cache, library, remote_types),
    push = function(..., direct = FALSE, .list = list())
      res_push(self, private, ..., direct = direct, .list = .list),
    when_complete = function() private$deferred
  ),

  private = list(
    remote_types = NULL,
    config = NULL,
    cache = NULL,
    library = NULL,
    deferred = NULL,
    state = NULL,
    dependencies = NULL,
    metadata = NULL,
    bar = NULL,

    delayed = list(),
    delayed_refs = character(),
    resolve_delayed = function(resolve)
      res__resolve_delayed(self, private, resolve),

    create_progress_bar = function()
      res__create_progress_bar(self, private),
    done_progress_bar = function()
      res__done_progress_bar(self, private),

    set_result = function(row_idx, value)
      res__set_result(self, private, row_idx, value),
    try_finish = function(resolve)
      res__try_finish(self, private, resolve)
  )
)

res_init <- function(self, private, config, cache, library,
                     remote_types) {

  "!DEBUG resolution init"
  private$config <- config
  private$cache <- cache
  private$library <- library
  private$remote_types <- remote_types %||% default_remote_types()
  private$metadata <- list(resolution_start = Sys.time())
  private$dependencies <- as_pkg_dependencies(config$get("dependencies"))
  private$bar <- private$create_progress_bar()

  self$result <- res_make_empty_df()

  private$state <- tibble(
    ref = character(),
    remote = list(),
    status = character(),
    direct = logical(),
    async_id = integer(),
    started_at = Sys.time()[FALSE])

  private$deferred <- asNamespace("pkgcache")$deferred$new(
    type = "resolution_queue",
    parent_resolve = function(value, resolve) {
      "!DEBUG resolution done"
      id <- value$id
      value <- value$value
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "OK"

      ## Rule out installed:: refs and packages with ?source param
      not_inst <- value$type != "installed"
      prms <- value[["params"]]
      if (!is.data.frame(value)) prms <- list(prms)
      want_source <- vlapply(prms, is_true_param, "source")
      want_reinst <- vlapply(prms, is_true_param, "reinstall")
      npkgs <- value$package[not_inst & ! want_source & ! want_reinst]

      ## Installed already? Resolve that as well
      if (!is.null(private$library) && length(npkgs)) {
        ml <- file.exists(file.path(private$library, npkgs))
        rc <- file.exists(file.path(.Library, npkgs)) &
          npkgs %in% recommended_packages()
        npkgs <- npkgs[ml | rc]
        if (length(npkgs))  {
          lib <- normalizePath(private$library, winslash = "/",
                               mustWork = FALSE)
          refs <- paste0("installed::", lib, "/", npkgs)
          refs <- setdiff(refs, private$state$ref)
          self$push(.list = parse_pkg_refs(refs))
        }
      }

      private$set_result(wh, value)
      private$try_finish(resolve)
    },

    parent_reject = function(value, resolve) {
      "!DEBUG resolution failed"
      id <- value$id
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "FAILED"
      rec <- private$state[wh,]
      fail_val <- list(
        ref = rec$ref,
        type = rec$remote[[1]]$type,
        package = rec$remote[[1]]$package %|z|% NA_character_,
        version = NA_character_,
        sources = NA_character_,
        direct = rec$direct,
        status = "FAILED",
        remote = rec$remote,
        error = list(value)
      )
      private$set_result(wh, fail_val)
      private$try_finish(resolve)
    })
}

res_push <- function(self, private, ..., direct, .list = .list) {
  new <- c(list(...), .list)

  ## Drop the ones already resolving up front
  new_refs <- vcapply(new, "[[", "ref")
  keep <- ! new_refs %in% c(private$state$ref, private$delayed_refs)
  new <- new[keep]
  new_refs <- new_refs[keep]

  ## Drop duplicates as well
  uni_refs <- !duplicated(new_refs)
  if (! all(uni_refs)) {
    new <- new[uni_refs]
    new_refs <- new_refs[uni_refs]
  }

  ## We do CRAN/BioC/standard in batches
  ## TODO: direct ones in one go as well
  batch_types <- setdiff(c("cran", "standard", "bioc", "installed"),
                         private$remote_types)
  delay <- vcapply(new, "[[", "type") %in% batch_types
  if (!direct && any(delay)) {
    refs <- vcapply(new[delay], "[[", "ref")
    private$delayed <- c(private$delayed, new[delay])
    private$delayed_refs <- c(private$delayed_refs, refs)
    new <- new[!delay]
    "!DEBUG pushing `sum(delay)` batch resolutions"
  }

  for (n in new) {
    "!DEBUG resolution push `n$ref`"
    dx <- resolve_remote(n, direct, private$config, private$cache,
                         private$dependencies,
                         remote_types = private$remote_types)

    private$state <- rbind(
      private$state,
      tibble(ref = n$ref, remote = list(n), status = NA_character_,
             direct = direct, async_id = dx$id, started_at = Sys.time())
    )

    dx$dx$then(private$deferred)
  }
}

res__resolve_delayed <- function(self, private, resolve) {
  n <- private$delayed
  private$delayed <- list()
  private$delayed_refs <- character()

  refs <- vcapply(n, "[[", "ref")
  done <- refs %in% private$state$ref
  n <- n[!done]
  refs <- vcapply(n, "[[", "ref")
  "!DEBUG resolving `length(private$delayed)` delayed remotes"

  if (length(n))  {
    types <- vcapply(n, "[[", "type")
    utypes <- unique(types)
    for (t in utypes) {
      n2 <- n[types == t]

      dx <- resolve_remote(n2, direct = FALSE, private$config,
                           private$cache, private$dependencies,
                           remote_types = private$remote_types)

      private$state <- rbind(
        private$state,
        tibble(ref = vcapply(n2, "[[", "ref"), remote = n2,
               status = NA_character_, direct = FALSE,
               async_id = dx$id, started_at = Sys.time())
      )
      dx$dx$then(private$deferred)
    }
  }

  private$try_finish(resolve)
}

res__set_result <- function(self, private, row_idx, value) {

  unknown <- if ("unknown_deps" %in% names(value)) value$unknown_deps
  if (is.null(unknown) && !is.null(attr(value, "unknown_deps"))) {
    unknown <- attr(value, "unknown_deps")
    attr(value, "unknown_deps") <- NULL
  }
  value <- value[setdiff(names(value), "unknown_deps")]

  if (is.data.frame(value)) {
    res__set_result_df(self, private, row_idx, value)
  } else {
    res__set_result_list(self, private, row_idx, value)
  }

  "!DEBUG resolution setting result, total: `nrow(self$result)`"
  if (length(unknown)) self$push(.list = parse_pkg_refs(unknown))
}

res__set_result_df <- function(self, private, row_idx, value) {
  # remove the ones that are already done
  done <- value$ref %in% self$result$ref
  value <- value[!done, ]

  # avoid removing the ones that are direct and already on the TODO list
  running <- intersect(value$ref, private$state$ref)
  avoid <-
    !value$direct[match(running, value$ref)] &
    private$state$direct[match(running, private$state$ref)]
  value <- value[! value$ref %in% running[avoid], ]

  if (nrow(value)) self$result <- res_add_df_entries(self$result, value)
}

res__set_result_list <- function(self, private, row_idx, value) {
  # already done?
  if (all(value$ref %in% self$result$ref)) return()
  # direct version already on the TODO list?
  if (value$ref %in% private$state$ref &&
      !value$direct &&
      private$state$direct[match(value$ref, private$state$ref)]) return()
  self$result <- res_add_df_entries(self$result, value)
}

res__try_finish <- function(self, private, resolve) {
  "!DEBUG resolution trying to finish with `nrow(self$result)` results"
  if (length(private$delayed)) return(private$resolve_delayed(resolve))
  if (all(! is.na(private$state$status))) {
    "!DEBUG resolution finished"
    private$metadata$resolution_end <- Sys.time()
    self$result$cache_status <-
      calculate_cache_status(self$result, private$cache)
    attr(self$result, "metadata") <- private$metadata
    class(self$result) <- c("pkg_resolution_result", class(self$result))
    private$done_progress_bar()
    resolve(self$result)
  }
}

resolve_remote <- function(remote, direct, config, cache, dependencies,
                           remote_types = NULL) {

  remote; direct; config; cache; dependencies; remote_types

  remote_types <- c(default_remote_types(), remote_types)

  type <- remote$type %||% unique(vcapply(remote, "[[", "type"))
  if (length(type) != 1) stop("Invalid remote or remote list, multiple types?")

  resolve <- remote_types[[type]]$resolve
  if (is.null(resolve)) {
    stop("Cannot resolve type", format_items(type))
  }

  id <- get_id()
  dx <- async(resolve)(
    remote, direct = direct, config = config, cache = cache,
    dependencies = dependencies
  )$
  then(function(value) {
      # if 'dep_types' was already added by the resolution of the remote
      # type (like any::) then we just keep that. Otherwise calculate from
      # 'dependencies'
      if (is.null(value[["dep_types"]])) {
        # remote is either direct or indirect dependency, depending on
        # 'direct', and the rest are all indirect
        if (NROW(value)) {
          value[["dep_types"]] <- list(dependencies[[2]])
          if (!is.null(remote$package)) {
            value$dep_types[value$package == remote$package] <-
              list(dependencies[[2 - direct]])
          } else {
            value$dep_types[value$ref == remote$ref] <-
              list(dependencies[[2 - direct]])
          }
        } else {
          value[["dep_types"]] <- list()
        }
      }
      list(value = value, id = id)
  })$
    catch(error = function(err) {
      err$id <- id
      stop(err)
    })

  list(dx = dx, id = id)
}

resolve_from_description <- function(path, sources, remote, direct,
                                     config, cache, dependencies) {

  dsc <- desc(file = path)
  deps <- resolve_ref_deps(
    dsc$get_deps(),
    dsc$get("Remotes")[[1]],
    dsc$get(extra_config_fields(dsc$fields()))
  )

  rversion <- tryCatch(
    get_minor_r_version(dsc$get_built()$R),
    error = function(e) "*"
  )

  platform <- if (dsc$has_fields("Built")) {
    built <- dsc$get_built()
    archs <- gsub(" ", "", dsc$get("Archs"))
    if (built$OStype == "windows") {
      if (is.na(archs) || archs == "i386,x64" || archs == "x64,i386") {
        "i386+x86_64-w64-mingw32"
      } else {
        built$Platform
      }
    } else {
      built$Platform
    }
  } else {
    "source"
  }

  nc <- dsc$get_field("NeedsCompilation", NA)
  if  (!is.na(nc)) nc <- tolower(nc) %in% c("true", "yes")

  unknown <- deps$ref[deps$type %in% dependencies]

  meta <- c(
    RemotePkgRef = remote$ref,
    RemoteType = remote$type,
    RemoteSha = NULL,                   # TODO
    RemoteUrl = NULL,                   # TODO
    RemoteUsername = NULL,              # TODO
    RemoteRepo = NULL,                  # TODO
    RemoteBranch = NULL                 # TODO
  )

  list(
    ref = remote$ref,
    type = remote$type,
    direct = direct,
    status = "OK",
    package = dsc$get_field("Package"),
    version = dsc$get_field("Version"),
    license = dsc$get_field("License", NA_character_),
    needscompilation = nc,
    md5sum = dsc$get_field("MD5sum", NA_character_),
    built = dsc$get_field("Built", NA_character_),
    platform = platform,
    rversion = rversion,
    deps = list(deps),
    sources = sources,
    remote = list(remote),
    unknown_deps = setdiff(unknown, "R"),
    extra = list(list(description = dsc)),
    metadata = meta,
    params = list(remote$params),
    sysreqs = dsc$get_field("SystemRequirements", "")
  )
}

# TODO: Parse remotes and Config/Needs/* fields

resolve_from_metadata <- function(remotes, direct, config, cache,
                                  dependencies) {

  remotes; direct; config; cache; dependencies

  ## Single remote, or a list of remotes
  if ("ref" %in% names(remotes)) {
    packages <- remotes$package
    refs <- remotes$ref
    types <- remotes$type
    params <- list(remotes$params)
  } else  {
    packages <- vcapply(remotes, "[[", "package")
    refs <- vcapply(remotes,  "[[", "ref")
    types <-  vcapply(remotes, "[[", "type")
    params <- lapply(remotes, "[[", "params")
  }

  if (!direct) dependencies <- dependencies$indirect
  "!DEBUG resolving `length(refs)` batch resolution"
  cache$metadata$async_deps(packages, dependencies = dependencies)$
    then(function(data) {
      cols <-  c(
        "ref", "type", "status", "package", "version", "license",
        "needscompilation", "priority", "md5sum", "platform",
        "rversion", "repodir", "target", "deps", "sources", "mirror",
        "filesize", "sha256", "sysreqs")

      res <- data[cols]
      res$built <- data[["built"]] %||% rep(NA_character_, nrow(res))
      idx <- match(res$package, packages)
      res$ref[!is.na(idx)] <- na.omit(refs[idx])
      res$repotype <- res$type
      res$type[] <- "standard"
      res$type[!is.na(idx)] <- na.omit(types[idx])
      res$needscompilation <-
        tolower(res$needscompilation) %in% c("yes", "true")
      res$direct <- direct & res$ref %in% refs
      res$params <- replicate(nrow(res), character())
      res$params[!is.na(idx)] <- params[na.omit(idx)]

      res$metadata <- get_standard_metadata(res)

      # Drop binaries if source package was requested
      want_source <- vlapply(res$params, is_true_param, "source")
      todrop <- res$platform != "source" & want_source
      if (any(todrop)) res <- res[!todrop, ]

      if (length(bad <- attr(data, "unknown"))) {
        idx <- match(bad, packages)
        bad[!is.na(idx)] <- na.omit(refs[idx])
        failed <- make_failed_resolution(
          bad, ifelse(!is.na(idx), types[idx], "standard"),
          direct & bad %in% refs)
        res <- rbind_expand(res, res_add_defaults(failed))
      }

      res
    })
}

get_standard_metadata <- function(tab) {
  meta <- replicate(nrow(tab), character(), simplify = FALSE)
  for (i in seq_len(nrow(tab))) {
    meta[[i]] <-
      c(RemoteType = tab$type[i],
        RemotePkgRef = tab$ref[i],
        RemoteRef = tab$ref[i],
        RemoteRepos = tab$mirror[i],
        RemotePkgPlatform = tab$platform[i],
        RemoteSha = tab$version[i])
  }
  meta
}

make_failed_resolution <- function(refs, type, direct) {
  rstr <- paste(refs, collapse = ", ")
  err <- structure(
    list(message = paste0("Can't find package called ", rstr, ".")),
    class = c("error", "condition"))
  tibble(
    ref = refs,
    type = type,
    package = sub("^[a-z]+::", "", refs),
    version = NA_character_,
    sources = replicate(length(refs), NA_character_, simplify = FALSE),
    direct = direct,
    status = "FAILED",
    remote = parse_pkg_refs(refs),
    error = replicate(length(refs), err, simplify = FALSE)
  )
}

#' @export

`[.pkg_resolution_result` <- function (x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "pkg_resolution_result")
  NextMethod("[")
}
