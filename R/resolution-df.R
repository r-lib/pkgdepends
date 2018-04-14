
res_make_empty_df <- function() {
  tibble(
    ref      = character(),
    type     = character(),
    direct   = logical(),
    status   = character(),             # "OK" or "FAILED"
    package  = character(),
    version  = character(),
    license  = character(),
    needscompilation
             = logical(),
    priority = character(),
    md5sum   = character(),
    filesize = integer(),
    built    = character(),
    platform = character(),             # "source" or platform string
    rversion = character(),             # * or version number (prefix)
    repodir  = character(),
    target   = character(),
    deps     = list(),
    sources  = list(),                  # list of URLs
    remote   = list(),                  # parsed remote ref
    error    = list(),                  # list of errors
    metadata = list(),                  # named character of entries
    extra    = list()                   # any extra data (e.g. GitHub sha)
  )
}

res_df_defaults <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- list(
        direct   = FALSE,
        status   = "OK",
        license  = NA_character_,
        needscompilation
                 = TRUE,
        priority = NA_character_,
        md5sum   = NA_character_,
        filesize = NA_integer_,
        built    = NA_character_,
        platform = "source",
        rversion = "*",
        repodir  =  "src/contrib",
        target   =
          quote(file.path(repodir, paste0(package, "_", version, ".tar.gz"))),
        deps     = list(make_null_deps()),
        remote   = quote(parse_remotes(ref)),
        error    = list(list()),
        metadata = list(list()),
        extra    = list(list())
      )
    }
    data
  }
})

res_df_entry_types <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- vcapply(res_make_empty_df(), class)
    }
    data
  }
})

res_df_must_have <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- setdiff(names(res_make_empty_df()), names(res_df_defaults()))
    }
    data
  }
})

res_check_entry <- function(ent) {
  ## Some columns are required
  assert_that(is.list(ent), all_named(ent))

  if (length(miss <- setdiff(res_df_must_have(), names(ent)))) {
    stop("Entries missing from remote: ", format_items(miss))
  }

  ent <- lapply(ent, eval, envir = ent)

  ent_types <- vcapply(ent, class)
  exp_types <- res_df_entry_types()[names(ent)]
  if (any(bad <- ent_types != exp_types)) {
    items <- paste0(names(ent)[bad], " (", ent_types[bad], ", expected ",
                    exp_types[bad], ")")
    stop("Wrong entry types: ", format_items(items))
  }

  invisible(ent)
}

#' @param df Resolution data frame (tibble, really).
#' @param entries List of entries to add.

res_add_df_entries <- function(df, entries) {
  entries <- lapply(entries, modifyList, x = res_df_defaults())
  entries <- lapply(entries, res_check_entry)
  entries <- do.call(rbind, lapply(entries, as.tibble))
  as.tibble(rbind(df, entries))[names(df)]
}
