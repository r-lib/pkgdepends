
#' @export

parse_remote.remote_specs_cran <- function(specs) {

  parsed_specs <- re_match(specs, cran_rx())
    
  parsed_specs$ref <- parsed_specs$.text
  cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
  parsed_specs <- parsed_specs[, cn]
  parsed_specs$type <- "cran"
  lapply(
    seq_len(nrow(parsed_specs)),
    function(i) as.list(parsed_specs[i,])
  )
}

#' @export

resolve_remote.remote_ref_cran <- function(remote) {
  TODO
}

#' @export

download_remote.remote_ref_cran <- function(remote) {
  TODO
}
