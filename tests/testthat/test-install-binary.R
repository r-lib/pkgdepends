

test_that("make_install_process error", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("foobar\n", file = tmp)

  expect_error(
    make_install_process(tmp, lib = tempdir()),
    "Cannot extract", class = "install_input_error"
  )
})

test_that("install_extracted_binary, add_metadata", {
  pkg <- binary_test_package("foo")
  type <- detect_package_archive_type(pkg)
  pkg_cache <- withr::local_tempdir()

  p <- if (type == "zip") {
    make_unzip_process(pkg, exdir = pkg_cache)
  } else {
    make_untar_process(pkg, exdir = pkg_cache)
  }

  p$wait(50000)
  p$kill()
  expect_snapshot(dir(pkg_cache))

  lib <- withr::local_tempdir()
  mkdirp(lib_cache <- file.path(lib, "_cache"))
  install_extracted_binary(
    basename(pkg),
    lib_cache,
    pkg_cache,
    lib,
    metadata = c(Foo = "bar", Foobar = "baz")
  )

  x <- callr::r(function(l) {
    library("foo", lib.loc = l)
    foo::foo()
  }, list(lib))
  expect_null(x)

  expect_snapshot({
    d <- desc::desc(file.path(lib, "foo"))
    d$get_field("Package")
    d$get_field("Foo")
    d$get_field("Foobar")
  })

  rds <- readRDS(file.path(lib, "foo", "Meta", "package.rds"))
  expect_snapshot(rds$DESCRIPTION[c("Package", "Foo", "Foobar")])

  # add more metadata
  add_metadata(
    file.path(lib, "foo"),
    c(Another = "field")
  )

  expect_snapshot({
    d <- desc::desc(file.path(lib, "foo"))
    d$get_field("Package")
    d$get_field("Another")
  })

  rds <- readRDS(file.path(lib, "foo", "Meta", "package.rds"))
  expect_snapshot(rds$DESCRIPTION[c("Package", "Another")])

  # install again, to test overwriting. we need to extract again,
  # because it was probably moved
  p <- if (type == "zip") {
    make_unzip_process(pkg, exdir = pkg_cache)
  } else {
    make_untar_process(pkg, exdir = pkg_cache)
  }

  p$wait(50000)
  p$kill()
  expect_snapshot(dir(pkg_cache))

  install_extracted_binary(
    basename(pkg),
    lib_cache,
    pkg_cache,
    lib,
    metadata = c(Foo = "bar2", Foobar = "baz2")
  )

  expect_snapshot({
    d <- desc::desc(file.path(lib, "foo"))
    d$get_field("Package")
    d$get_field("Foo")
    d$get_field("Foobar")
  })

  rds <- readRDS(file.path(lib, "foo", "Meta", "package.rds"))
  expect_snapshot(rds$DESCRIPTION[c("Package", "Foo", "Foobar")])

  # check that MD5 sums were updated on Windows
  if (is_windows()) {
    tools::checkMD5sums(dir = file.path(lib, "foo"))
  }
})

test_that("add_metadata error", {
  tmp <- withr::local_tempdir()
  expect_snapshot(
    error = TRUE,
    transform = transform_tempdir,
    add_metadata(tmp, c(foo = "bar"))
  )
})

test_that("failing to move and/or remove existing installation", {
  # this is challenging to test, but we should do it, nevertheless
  skip("TODO")
})
