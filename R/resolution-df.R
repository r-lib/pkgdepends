
res_make_empty_df <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- data_frame(
        ref      = character(),
        type     = character(),
        direct   = logical(),
        directpkg= logical(),
        status   = character(),         # "OK" or "FAILED"
        package  = character(),
        version  = character(),
        license  = character(),
        needscompilation
                 = logical(),
        priority = character(),
        md5sum   = character(),
        sha256   = character(),
        filesize = integer(),
        built    = character(),
        platform = character(),         # "source" or platform string
        rversion = character(),         # * or version number (prefix)
        repotype = character(),
        repodir  = character(),
        target   = character(),
        deps     = list(),
        mirror   = character(),         # for CRAN/BioC
        sources  = list(),              # list of URLs
        remote   = list(),              # parsed remote ref
        error    = list(),              # list of errors
        metadata = list(),              # named character of entries
        extra    = list(),              # any extra data (e.g. GitHub sha)
        dep_types= list(),
        params   = list(),
        sysreqs  = character()
      )
    }
    data
  }
})

res_df_defaults <- local({
  data <- NULL
  function() {
    if (is.null(data)) {
      data <<- list(
        direct   = FALSE,
        directpkg= FALSE,
        status   = "OK",
        license  = NA_character_,
        needscompilation
                 = TRUE,
        priority = NA_character_,
        md5sum   = NA_character_,
        sha256   = NA_character_,
        filesize = NA_integer_,
        built    = NA_character_,
        platform = "source",
        rversion = "*",
        repotype = NA_character_,
        repodir  =  "src/contrib",
        target   =
          quote(file.path("src/contrib", paste0(package, "_", version, ".tar.gz"))),
        deps     = list(make_null_deps()),
        remote   = quote(parse_pkg_refs(ref)),
        error    = list(list()),
        metadata = list(list()),
        mirror   = NA_character_,
        extra    = list(list()),
        dep_types= list(list()),
        params   = list(character()),
        sysreqs  = NA_character_
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

res_check_entries <- function(ent) {
  ## Some columns are required
  assert_that(is.data.frame(ent))

  if (length(miss <- setdiff(res_df_must_have(), names(ent)))) {
    stop("Entries missing from remote: ", format_items(miss))
  }

  ent_types <- vcapply(ent, class)
  exp_types <- res_df_entry_types()[names(ent)]
  if (any(bad <- ent_types != exp_types)) {
    items <- paste0(names(ent)[bad], " (", ent_types[bad], ", expected ",
                    exp_types[bad], ")")
    stop("Wrong entry types: ", format_items(items))
  }

  invisible(ent)
}

#' @noRd
#' @param df Resolution data frame.
#' @param entries List of entries to add.

res_add_df_entries <- function(df, entries) {
  if (!is.data.frame(entries)) entries <- res_one_row_df(entries)
  entries <- res_add_defaults(entries)
  ret <- as_data_frame(rbind(df, entries))[names(df)]
  direct <- ret$package[ret$direct]
  ret$directpkg <- ret$package %in% direct
  ret
}

res_one_row_df <- function(l) {
  assert_that(is.list(l) && all_named(l))
  samp <- res_make_empty_df()[names(l)]
  bad <- vlapply(samp, is.list) & !vlapply(l, is.list)
  l[bad] <- lapply(l[bad], list)
  as_data_frame(l)
}

res_add_defaults <- function(df) {
  if (length(bad <- setdiff(res_df_must_have(), names(df)))) {
    stop("Entries missing from remote: ", format_items(bad))
  }

  all_types <- res_df_entry_types()
  miss <- setdiff(names(all_types), names(df))
  def <- lapply(res_df_defaults()[miss], eval, envir = df,
                enclos = environment())
  df[names(def)] <- def
  df <- df[, names(all_types)]

  if ("filesize" %in% names(df)) df$filesize <- as.integer(df$filesize)

  ent_types <- vcapply(df, typeof)
  exp_types <- all_types[names(df)]
  if (any(bad <- ent_types != exp_types)) {
    items <- paste0(names(df)[bad], " (", ent_types[bad], ", expected ",
                    exp_types[bad], ")")
    stop("Wrong entry types: ", format_items(items))
  }

  df
}
