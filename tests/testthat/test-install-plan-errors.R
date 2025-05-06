test_that("uncompress error", {
  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  pkgzip <- file.path(tmp, "foo-t.zip")
  zip::zipr(pkgzip, test_path("fixtures", "foo"))
  bts <- readBin(pkgzip, "raw", file.size(pkgzip))
  bts <- head(bts, as.integer(length(bts) / 2))
  writeBin(bts, pkgzip)

  plan <- data_frame(
    type = "local",
    binary = FALSE,
    packaged = FALSE,
    dependencies = list(character()),
    needscompilation = TRUE,
    file = pkgzip,
    package = "foo"
  )

  expect_snapshot(
    error = TRUE,
    transform = function(x) sub("'.*/foo-t.zip", ".../foo-t.zip", x),
    install_package_plan(plan, lib = tempdir())
  )
})

test_that("build error", {
  withr::local_envvar(PKG_OMIT_TIMES = "true")
  pkg <- source_test_package("foo2")

  quo <- substitute(
    {
      plan <- data.frame(
        stringsAsFactors = FALSE,
        type = "local",
        binary = FALSE,
        dependencies = I(list(character())),
        file = pkg,
        needscompilation = TRUE,
        package = "foo"
      )

      lib <- withr::local_tempdir()
      pkgdepends:::install_package_plan(plan, lib = lib)
    },
    list(pkg = pkg)
  )

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(deparse(quo), tmp)

  out <- tempfile()
  on.exit(unlink(out), add = TRUE)
  res <- callr::rscript(
    tmp,
    stdout = out,
    stderr = out,
    fail_on_status = FALSE,
    show = FALSE
  )

  # We can't match the whole output or even the error message, because
  # different compilers have different output. Nevertheless this should
  # appear in the output.
  expect_match(res$stdout, "(return R_NilValue|error: expected)")
})

test_that("packaging error", {
  withr::local_envvar(PKG_OMIT_TIMES = "true")
  tmp <- withr::local_tempdir()
  pkgzip <- file.path(tmp, "foo3-t.zip")
  zip::zipr(pkgzip, test_path("fixtures", "foo3"))

  quo <- substitute(
    {
      plan <- data.frame(
        stringsAsFactors = FALSE,
        type = "local",
        binary = FALSE,
        dependencies = I(list(character())),
        file = pkgzip,
        needscompilation = TRUE,
        package = "foo3",
        packaged = FALSE
      )

      lib <- withr::local_tempdir()
      pkgdepends:::install_package_plan(plan, lib = lib)
    },
    list(pkgzip = pkgzip)
  )

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(deparse(quo), tmp)

  out <- tempfile()
  on.exit(unlink(out), add = TRUE)
  res <- callr::rscript(
    tmp,
    stdout = out,
    stderr = out,
    fail_on_status = FALSE,
    show = FALSE
  )

  expect_true(
    grepl("not in valid DCF format", res$stdout, fixed = TRUE) ||
      grepl("Error in read.dcf", res$stdout, fixed = TRUE)
  )
})

test_that("warn_for_long_paths", {
  # TODO: this is challenging to test, because it is hard to create these
  # long paths on Windows.
})
