
test_that("can package a tree", {

  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  file.copy(test_path("fixtures", "foo"), tmp, recursive = TRUE)

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data_frame(
    ref = paste0("local::./foo"),
    type = "local",
    direct = TRUE,
    status = "OK",
    package = "foo",
    version = "0.0.0.9000",
    binary = FALSE,
    packaged = FALSE,
    dependencies = list(character()),
    file = tmp,
    needscompilation = FALSE,
    install_args = ""
  )

  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
})

test_that("can package a compressed tree", {

  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  pkgzip <- file.path(tmp, "foo-t.zip")
  zip::zipr(pkgzip, test_path("fixtures", "foo"))

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data.frame(
    stringsAsFactors = FALSE,
    ref = paste0("local::./foo"),
    type = "local",
    direct = TRUE,
    status = "OK",
    package = "foo",
    version = "0.0.0.9000",
    binary = FALSE,
    packaged = FALSE,
    dependencies = I(list(character())),
    file = pkgzip,
    vignettes = FALSE,
    needscompilation = FALSE,
    metadata = I(list(character()))
  )

  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
})


test_that("can package a source package", {

  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  pkg <- source_test_package("foo")

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data.frame(
    stringsAsFactors = FALSE,
    ref = paste0("local::./foo"),
    type = "local",
    direct = TRUE,
    status = "OK",
    package = "foo",
    version = "0.0.0.9000",
    binary = FALSE,
    dependencies = I(list(character())),
    file = pkg,
    vignettes = FALSE,
    needscompilation = FALSE,
    metadata = I(list(character()))
  )

  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
  expect_true(file.exists(file.path(lib, "foo", "installed-file")))
})

test_that("add_recursive_dependencies", {
  plan <- data_frame(
    package = c("p1", "p2", "p3"),
    type = "cran",
    binary = TRUE,
    dependencies = list("p2", "p3", character()),
    file = NA_character_,
    needscompilation = FALSE
  )

  expect_equal(add_recursive_dependencies(plan), plan)

  plan$binary <- FALSE
  expect_equal(add_recursive_dependencies(plan), plan)

  plan$binary <- c(FALSE, TRUE, FALSE)
  expect_snapshot(add_recursive_dependencies(plan))
  expect_snapshot(add_recursive_dependencies(plan)$dependencies)
})
