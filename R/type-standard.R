
## ------------------------------------------------------------------------
## API

#' @export

parse_remote.remote_specs_standard <- function(specs, config, ...) {

  ## This is the same as CRAN, but possibly with standard::
  parsed_specs <- re_match(specs, standard_rx())
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "standard"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

#' @export

resolve_remote.remote_ref_standard <- function(remote, config, ...,
                                                cache) {
  force(remote)

  cache$crandata <- cache$crandata %||% update_crandata_cache(config)
  cache$biocdata <- cache$biocdata %||% update_biocdata_cache(config)

  ## First we try CRAN, then BioC, then fail
  cache$crandata$
    then(function(cacheresult) {
      type_cran_resolve_from_cache(remote, config, cacheresult)
    })$
    then(function(cran_resolution) {
      cache$biocdata$then(function(cacheresult) {
        if (cran_resolution$status == "OK") {
          cran_resolution
        } else {
          type_bioc_resolve_from_cache(remote, config, cacheresult)
        }
      })
    })
}
