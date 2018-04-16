
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
    remote_types = private$remote_types, cli = private$cli)

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

resolution <- R6Class(
  "resolution",
  public = list(
    result = NULL,
    initialize = function(config, cache, remote_types = NULL, cli = NULL)
      res_init(self, private, config, cache, remote_types, cli),
    push = function(..., direct = FALSE, .list = list())
      res_push(self, private, ..., direct = direct, .list = .list),
    when_complete = function() private$deferred
  ),

  private = list(
    remote_types = NULL,
    config = NULL,
    cache = NULL,
    deferred = NULL,
    state = NULL,
    cli = NULL,
    dependencies = NULL,
    metadata = NULL,

    set_result = function(row_idx, value)
      res__set_result(self, private, row_idx, value),
    try_finish = function(resolve)
      res__try_finish(self, private, resolve)
  )
)

res_init <- function(self, private, config, cache, remote_types, cli) {

  "!DEBUG resolution init"
  private$config <- config
  private$cache <- cache
  private$remote_types <- remote_types %||% default_remote_types()
  private$cli <- cli %||% cli::cli
  private$metadata <- list(resolution_start = Sys.time())

  private$dependencies <- interpret_dependencies(config$dependencies)

  self$result <- res_make_empty_df()

  private$state <- tibble(
    ref = character(),
    remote = list(),
    status = character(),
    async_id = integer())

  private$deferred <- deferred$new(
    type = "resolution_queue",
    parent_resolve = function(value, resolve, id) {
      "!DEBUG resolution done"
      wh <- match(id, private$state$async_id)
      private$state$status[wh] <- "OK"
      private$set_result(wh, value)
      private$try_finish(resolve)
    },
    parent_reject = function(value, resolve, id) {
      "!DEBUG resolution failed"
      wh <- match(id, private$state$async_id)
      private$state$status[wh] <- "FAILED"
      ## TODO: create proper FAILED value
      private$set_result(wh, value)
      private$try_finish(resolve)
    })
}

res_push <- function(self, private, ..., direct, .list = .list) {
  new <- c(list(...), .list)
  "!DEBUG resolution push `length(new)`"

  for (n in new) {
    ## Maybe this is already resolving
    if (n$ref %in% private$state$ref) next

    resolve <- private$remote_types[[n$type]]$resolve
    if (is.null(resolve)) stop("Cannot resolve type", format_items(n$type))
    dx <- resolve(
      n, direct = direct, config = private$config, cache = private$cache,
      dependencies = private$dependencies[[direct + 1L]])

    ## We always return a deferred
    if (!is_deferred(dx)) dx <- async_constant(dx)

    private$state <- rbind(
      private$state,
      tibble(ref = n$ref, remote = list(n), status = NA_character_,
             async_id = dx$get_id()))

    dx$then(private$deferred)
  }
}

res__set_result <- function(self, private, row_idx, value) {
  unknown <- if ("unknown_deps" %in% names(value)) value$unknown_deps
  value <- value[setdiff(names(value), "unknown_deps")]
  self$result <- res_add_df_entries(self$result, value)
  "!DEBUG resolution setting result, total: `nrow(self$result)`"
  if (length(unknown)) self$push(.list = parse_remotes(unknown))
}

res__try_finish <- function(self, private, resolve) {
  "!DEBUG resolution trying to finish with `nrow(self$result)` results"
  if (all(! is.na(private$state$status))) {
    "!DEBUG resolution finished"
    private$metadata$resolution_end <- Sys.time()
    attr(self$result, "metadata") <- private$metadata
    class(self$result) <- c("remotes_resolution", class(self$result))
    resolve(self$result)
  }
}

resolve_from_description <- function(path, sources, remote, direct,
                                     config, cache, dependencies) {

  tryCatch({
    dsc <- desc(file = path)

    deps <- resolve_ref_deps(
      dsc$get_deps(), dsc$get("Remotes")[[1]], dependencies)

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

    remote$description <- dsc

    list(
      ref = remote$ref,
      type = remote$type,
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
      unknown_deps = setdiff(deps$ref, "R")
    )
  }, error = function(err) {

    list(
      ref = remote$ref,
      type = remote$type,
      status = "FAILED",
      error = list(err),
      package = dsc$get_field("Package", NA_character_),
      version = dsc$get_field("Version", NA_character_),
      license = dsc$get_field("License", NA_character_),
      sources = list(NA_character_)
    )
  })
}

resolve_from_metadata <- function(remote, direct, config, cache,
                                  dependencies) {

  remote; direct; config; cache; dependencies

  cache$metadata$async_deps(remote$package, dependencies = dependencies)$
    then(function(data) {
      cols <-  c(
        "ref", "type", "status", "package", "version", "license",
        "needscompilation", "priority", "md5sum", "built", "platform",
        "rversion", "repodir", "target", "deps", "sources")
      res <- data[cols]
      res$ref[res$package == remote$package] <- remote$ref
      res$type <- "standard"
      res$type[res$package == remote$package] <- remote$type
      res$needscompilation <-
        tolower(res$needscompilation) %in% c("yes", "true")
      res$direct <- direct & res$ref == remote$ref
      res
    })
}
