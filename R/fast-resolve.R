
remotes__add_fast_refs <- function(self, private, remotes, refs) {
  cache <- private$resolution$cache
  cache$crandata <- cache$crandata %||% update_crandata_cache(private$config)

  if (!is.null(remotes)) {
    private$resolution$fast$remotes <-
      c(private$resolution$fast$remotes, remotes)
  }
  if (!is.null(refs)) {
    private$resolution$fast$refs <-
      c(private$resolution$afst$refs, refs)
  }
}

remotes__fast_resolve <- function(self, private) {
  return()
  ## parse all refs and put them in remotes
  refs <- private$resolution$fast$refs
  rems <- private$resolution$fast$remotes
  rems <- private$resolution$fast$remotes <- c(rems, parse_remotes(refs))

  ## if bioc types, start getting bioc metadata
  types <- vcapply(rems, "[[", "type")
  if ("bioc" %in% types) {
    cache$biocdata <- cache$biocdata %||% update_biocdata_cache(config)
  }

  ## parse cran types
  remotes__fast_resolve

  ## try parse standard types via CRAN

  ## if anything left, try with BIOC
  
}
