
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
    dsc <- withr::with_libpaths(
      remote$library,
      desc(package = remote$package)
    )

    deps <- resolve_ref_deps(
      dsc$get_deps(), dsc$get("Remotes")[[1]], config$dependencies)

    files <- list(
      source = character(),
      target = NA_character_,
      platform = dsc$get_built()$Platform %|z|% "*",
      rversion = get_minor_r_version(dsc$get_built()$R),
      dir = NA_character_,
      package = dsc$get("Package")[[1]],
      version = dsc$get("Version")[[1]],
      deps = deps,
      status = "OK"
    )

    remote$description <- dsc

    structure(
      list(files = list(files), remote = remote, status = "OK"),
      class = c("remote_resolution_installed", "remote_resolution")
    )
  }

  ## ----------------------------------------------------------------------

  download_remote.remote_resolution_installed <<- function(resolution,
                                                           config, ...,
                                                           cache) {
    status <- make_dl_status("Had", NA_character_, NA_character_,
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
