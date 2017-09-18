
#' @export
#' @importFrom rematch2 re_match

parse_remote.remote_specs_github <- function(specs) {

  pds <- re_match(specs, github_rx())
    
  pds$ref <- pds$.text
  cn <- setdiff(colnames(pds), c(".match", ".text"))
  pds <- pds[, cn]
  pds$type <- "github"
  pds$package <- ifelse(nzchar(pds$package), pds$package, pds$repo)
  lapply(
    seq_len(nrow(pds)),
    function(i) as.list(pds[i,])
  )
}

#' @export

resolve_remote.remote_ref_github <- function(remote) {
  TODO
}

#' @export

download_remote.remote_ref_github <- function(remote) {
  TODO
}
