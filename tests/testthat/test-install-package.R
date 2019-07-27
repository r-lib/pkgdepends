
context("package a tree")

test_that("can package a tree", {

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(test_path("foo"), tmp, recursive = TRUE)

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data.frame(
    stringsAsFactors = FALSE,
    ref = paste0("local::./foo"), type = "local", direct = TRUE,
    status = "OK", package = "foo", version = "0.0.0.9000",
    binary = FALSE, packaged = FALSE, dependencies = I(list(character())),
    file = tmp, vignettes = FALSE, needscompilation = FALSE,
    metadata = I(list(character())))

  expect_error(install_package_plan(plan, lib = lib), NA)
  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
})

test_that("can package a compressed tree", {

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  pkgzip <- file.path(tmp, "foo-tree.zip")
  zip::zipr(pkgzip, test_path("foo"))

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data.frame(
    stringsAsFactors = FALSE,
    ref = paste0("local::./foo"), type = "local", direct = TRUE,
    status = "OK", package = "foo", version = "0.0.0.9000",
    binary = FALSE, packaged = FALSE, dependencies = I(list(character())),
    file = pkgzip, vignettes = FALSE, needscompilation = FALSE,
    metadata = I(list(character())))

  expect_error(install_package_plan(plan, lib = lib), NA)
  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
})
