test_that("folders with potentially problematic characters", {
  skip_on_cran()
  local_cli_config()

  tmp <- tempfile()
  on.exit(tryCatch(unloadNamespace("foo"), error = identity), add = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  on.exit(environment(need_internal_tar)$internal <- NULL, add = TRUE)

  pkg <- binary_test_package("foo")

  folders <- c(
    "s p a c e s",
    "\u00fa\u00e1\u00f6\u0151\u00e9",
    "s' p' a' c' e' s'"
  )

  skipped <- 0

  for (f in folders) {
    error <- FALSE
    tryCatch(
      {
        if ("foo" %in% loadedNamespaces()) unloadNamespace("foo")
        unlink(tmp, recursive = TRUE)
        dir.create(tmp)
        dir.create(file.path(tmp, f))
        libdir <- dir(tmp)
        libpath <- file.path(tmp, libdir)
      },
      warning = function(e) error <<- TRUE,
      error = function(e) error <<- TRUE
    )
    if (error) {
      skipped <- skipped + 1
      next
    }

    ## Reset this
    environment(need_internal_tar)$internal <- NULL

    suppressMessages(
      install_binary(pkg, lib = libpath, quiet = TRUE)
    )
    library("foo", lib.loc = libpath)
    expect_equal(foo::foo(), NULL)
    unloadNamespace("foo")

    ## Make sure tar is internal
    unlink(tmp, recursive = TRUE)
    dir.create(tmp)
    dir.create(libpath)
    environment(need_internal_tar)$internal <- NULL
    withr::with_envvar(
      c(TAR = NA),
      withr::with_path("foobar", action = "replace", {
        suppressMessages(install_binary(pkg, lib = libpath, quiet = TRUE))
      })
    )

    library("foo", lib.loc = libpath)
    expect_equal(foo::foo(), NULL)
    unloadNamespace("foo")
  }

  if (skipped) skip(paste(skipped, " path tests were skipped"))
})
