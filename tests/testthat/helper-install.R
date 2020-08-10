
local_binary_package <- function(pkgname, ..., envir = parent.frame()) {

  # All arguments must be named
  args <- list(...)
  stopifnot(length(args) == 0 || all_named(args))

  d <- create_temp_dir()
  pkgdir <- file.path(d, pkgname)
  dir.create(pkgdir)
  nms <- names(args)
  for (i in seq_along(args)) {
    dir.create(file.path(pkgdir, dirname(nms[[i]])), showWarnings = FALSE, recursive = TRUE)
    withr::with_connection(list(con = file(file.path(pkgdir, nms[[i]]), open = "wb")), {
      writeLines(args[[i]], con, sep = "\n")
    })
  }

  filename <- file.path(d, glue("{pkgname}.tgz"))
  withr::with_dir(
    dirname(filename),
    utils::tar(basename(filename), pkgname, compression = "gzip")
  )

  # We do not want to unlink files if we are calling this from the R console,
  # useful when debugging.
  is_globalenv <- identical(envir, globalenv())
  if (!is_globalenv) {
    withr::defer(unlink(d, recursive = TRUE), envir = envir)
  }
  filename
}

binary_test_package <- function(name) {
  mkdirp(tmp <- tempfile())
  file.copy(test_path(name), tmp, recursive = TRUE)
  zip_path <- system.file(package = "zip", "bin", .Platform$r_arch)
  withr::with_path(
    zip_path,
    pkgbuild::build(file.path(tmp, name), binary = TRUE, quiet = TRUE)
  )
}

source_test_package <- function(name) {
  mkdirp(tmp <- tempfile())
  file.copy(test_path(name), tmp, recursive = TRUE)
  pkgbuild::build(file.path(tmp, name), binary = FALSE, quiet = TRUE)
}

#' @importFrom callr r_process r_process_options

dummy_worker_process <- R6::R6Class(
  "dummy_worker_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    get_built_file = function() NA_character_
  )
)

make_dummy_worker_process <- function(n_iter = 10, sleep = 1, status = 0) {
  n_iter; sleep; status
  function(...) {
    dummy_worker_process$new(r_process_options(
      func = function(n_iter, sleep, status) {
                                        # nocov start
        for (i in seq_len(n_iter)) {
          cat("out ", i, "\n", sep = "")
          message("err ", i)
          Sys.sleep(sleep)
        }
        status
        .GlobalEnv$.Last <- function() {
          rm(list = ".Last", envir = .GlobalEnv)
          quit(save = "no", status = status)
        }
                                        # nocov end
      },
      args = list(n_iter = n_iter, sleep = sleep, status = status)
      ))
  }
}

make_install_plan <- function(ref, lib = .libPaths()[1]) {
  r <- pkg_plan$new(ref, lib = lib)
  r$resolve()
  r$solve()
  r$download_solution()
  r$get_install_plan()
}
