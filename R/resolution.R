
#' @importFrom prettyunits pretty_dt

remotes_resolve <- function(self, private) {
  "!DEBUG remotes_resolve (sync)"
  synchronise(self$async_resolve())
}

remotes_async_resolve <- function(self, private) {
  "!DEBUG remotes_resolve (async)"
  ## We remove this, to avoid a discrepancy between them
  private$downloads <- NULL
  private$solution <- NULL

  private$dirty <- TRUE
  private$resolution <- resolution$new(
    config = private$config, cache = private$cache,
    library = private$library, remote_types = private$remote_types)

  private$resolution$push(direct = TRUE, .list = private$remotes)

  private$resolution$when_complete()$
    then(function(x) {
      private$dirty <- FALSE
      x
    })
}

remotes_get_resolution <- function(self, private) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  private$resolution$result
}

remotes__subset_resolution <- function(self, private, which) {
  if (is.null(private$resolution$result)) stop("No resolution yet")
  res <- private$resolution$result[which, ]
  attr(res, "metadata")  <- attr(private$resolution$result, "metadata")
  res
}

resolution <- R6Class(
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
    update_progress_bar = function()
      res__update_progress_bar(self, private),
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
  private$dependencies <- interpret_dependencies(config$dependencies)
  private$bar <- private$create_progress_bar()

  self$result <- res_make_empty_df()

  private$state <- tibble(
    ref = character(),
    remote = list(),
    status = character(),
    direct = logical(),
    async_id = integer(),
    started_at = Sys.time()[FALSE])

  private$deferred <- deferred$new(
    type = "resolution_queue",
    parent_resolve = function(value, resolve, id) {
      "!DEBUG resolution done"
      wh <- which(id == private$state$async_id)
      private$state$status[wh] <- "OK"

      npkgs <- value$package[value$type != "installed"]
      ## Installed already? Resolve that as well
      if (!is.null(private$library) && length(npkgs)) {
        npkgs <- npkgs[file.exists(file.path(private$library, npkgs))]
        if (length(npkgs))  {
          lib <- normalizePath(private$library, winslash = "/",
                               mustWork = FALSE)
          refs <- paste0("installed::", lib, "/", npkgs)
          refs <- setdiff(refs, private$state$ref)
          self$push(.list = parse_remotes(refs))
        }
      }

      private$set_result(wh, value)
      private$try_finish(resolve)
      private$update_progress_bar()
    },

    parent_reject = function(value, resolve, id) {
      "!DEBUG resolution failed"
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
      private$update_progress_bar()
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
             direct = direct, async_id = dx$get_id(),
             started_at = Sys.time())
    )

    private$update_progress_bar()
    dx$then(private$deferred)
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
               async_id = dx$get_id(), started_at = Sys.time())
      )
      dx$then(private$deferred)
    }
    private$update_progress_bar()
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
  done <- value$ref %in% self$result$ref
  value <- if (is.data.frame(value)) value[!done, ] else value[!done]
  if (any(!done)) self$result <- res_add_df_entries(self$result, value)
  "!DEBUG resolution setting result, total: `nrow(self$result)`"
  if (length(unknown)) self$push(.list = parse_remotes(unknown))
}

res__try_finish <- function(self, private, resolve) {
  "!DEBUG resolution trying to finish with `nrow(self$result)` results"
  if (length(private$delayed)) return(private$resolve_delayed(resolve))
  if (all(! is.na(private$state$status))) {
    "!DEBUG resolution finished"
    private$metadata$resolution_end <- Sys.time()
    attr(self$result, "metadata") <- private$metadata
    class(self$result) <- c("remotes_resolution", class(self$result))
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

  async(resolve)(
    remote, direct = direct, config = config, cache = cache,
    dependencies = dependencies
  )$
    then(function(value) {
      value[["dep_types"]] <-
        if (NROW(value)) list(dependencies[[2-direct]]) else list()
      value
  })
}

resolve_from_description <- function(path, sources, remote, direct,
                                     config, cache, dependencies) {

  dsc <- desc(file = path)
  deps <- resolve_ref_deps(dsc$get_deps(), dsc$get("Remotes")[[1]])

  rversion <- tryCatch(
    get_minor_r_version(dsc$get_built()$R),
    error = function(e) "*"
  )

  platform <- tryCatch(
    dsc$get_built()$Platform %|z|% "source",
    error = function(e) "source"
  )

  nc <- dsc$get_field("NeedsCompilation", NA)
  if  (!is.na(nc)) nc <- tolower(nc) %in% c("true", "yes")

  unknown <- deps$ref[deps$type %in% dependencies]

  meta <- c(
    RemoteRef = remote$ref,
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
    metadata = meta
  )
}

resolve_from_metadata <- function(remotes, direct, config, cache,
                                  dependencies) {

  remotes; direct; config; cache; dependencies

  ## Single remote, or a list of remotes
  if ("ref" %in% names(remotes)) {
    packages <- remotes$package
    refs <- remotes$ref
    types <- remotes$type
  } else  {
    packages <- vcapply(remotes, "[[", "package")
    refs <- vcapply(remotes,  "[[", "ref")
    types <-  vcapply(remotes, "[[", "type")
  }

  if (!direct) dependencies <- dependencies$indirect
  "!DEBUG resolving `length(refs)` batch resolution"
  cache$metadata$async_deps(packages, dependencies = dependencies)$
    then(function(data) {
      cols <-  c(
        "ref", "type", "status", "package", "version", "license",
        "needscompilation", "priority", "md5sum", "platform",
        "rversion", "repodir", "target", "deps", "sources", "mirror",
        "filesize", "sha256")

      res <- data[cols]
      res$built <- data[["built"]] %||% rep(NA_character_, nrow(res))
      idx <- match(res$package, packages)
      res$ref[!is.na(idx)] <- na.omit(refs[idx])
      res$type[] <- "standard"
      res$type[!is.na(idx)] <- na.omit(types[idx])
      res$needscompilation <-
        tolower(res$needscompilation) %in% c("yes", "true")
      res$direct <- direct & res$ref %in% refs

      res$metadata <- get_standard_metadata(res)

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
        RemoteRef = tab$ref[i],
        RemoteRepos = tab$mirror[i],
        RemotePkgType = tab$platform[i],
        RemoteSha = tab$version[i])
  }
  meta
}

make_failed_resolution <- function(refs, type, direct) {
  err <- structure(
    list(message = "Cannot find standard package"),
    class = c("error", "condition"))
  tibble(
    ref = refs,
    type = type,
    package = NA_character_,
    version = NA_character_,
    sources = replicate(length(refs), NA_character_, simplify = FALSE),
    direct = direct,
    status = "FAILED",
    remote = parse_remotes(refs),
    error = replicate(length(refs), err, simplify = FALSE)
  )
}
