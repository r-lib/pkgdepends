
## ------------------------------------------------------------------------
## API

#' @export
parse_remote.remote_specs_installed <- NULL
#' @export
resolve_remote.remote_ref_installed <- NULL
#' @export
download_remote.remote_resolution_installed <- NULL

#' @importFrom desc desc

local({

  ## ----------------------------------------------------------------------

  parse_remote.remote_specs_installed <<- function(specs, config, ...) {
    parsed_specs <- re_match(specs, installed_rx())

    parsed_specs$ref <- parsed_specs$.text
    cn <- setdiff(colnames(parsed_specs), c(".match", ".text"))
    parsed_specs <- parsed_specs[, cn]
    parsed_specs$type <- "installed"
    lapply(
      seq_len(nrow(parsed_specs)),
      function(i) as.list(parsed_specs[i,])
    )
  }

  ## ----------------------------------------------------------------------

  resolve_remote.remote_ref_installed <<- function(remote, config, ...,
                                                   cache) {
    descr <- withr::with_libpaths(
      remote$library,
      desc(package = remote$package)
    )

    deps <- resolve_ref_deps(
      descr$get_deps(), descr$get("Remotes"), config$dependencies)

    files <- list(
      source = character(),
      target = NA_character_,
      platform = descr$get_built()$Platform %|z|% "*",
      rversion = get_minor_r_version(descr$get_built()$R),
      dir = NA_character_,
      package = descr$get("Package"),
      version = descr$get("Version"),
      deps = deps,
      status = "OK"
    )

    structure(
      list(files = list(files), remote = remote, status = "OK"),
      class = c("remote_resolution_installed", "remote_resolution")
    )
  }

  ## ----------------------------------------------------------------------

  download_remote.remote_resolution_installed <<- function(resolution,
                                                           config, ...,
                                                           cache) {
    status <- make_dl_status("Had", resolution$files[[1]], "", "",
                             bytes = NA)
    async_constant(list(status))
  }

  ## ----------------------------------------------------------------------
  ## Internal functions

  installed_rx <- function() {
    paste0(
      "^",
      "(?:installed::)?",
      "(?<library>.*)/",
      "(?<package>", package_name_rx(), ")",
      "$"
    )
  }

})
