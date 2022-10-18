
test_that("install_binary", {

  pkg <- binary_test_package("foo")
  lib <- withr::local_tempdir()

  expect_snapshot(
    install_binary(pkg, lib = lib, quiet = TRUE),
    transform = function(x) sub(dirname(pkg), "/...", x, fixed = TRUE)
  )

  x <- callr::r(function(l) {
    library("foo", lib.loc = l)
    foo::foo()
  }, list(lib))
  expect_null(x)

  # overwrite installed package
  expect_snapshot(
    install_binary(pkg, lib = lib, quiet = TRUE),
    transform = function(x) sub(dirname(pkg), "/...", x, fixed = TRUE)
  )
})

test_that("install_binary works for simultaneous installs", {
  skip_on_cran()

  pkg <- binary_test_package("foo")
  lib <- withr::local_tempdir()

  processes <- list()
  num <- 5

  # install and load foo here to test loaded DLLs in another process
  suppressMessages(
    install_binary(pkg, lib = lib, quiet = TRUE)
  )
  x <- callr::r(function(l) {
    library("foo", lib.loc = l)
    foo::foo()
  }, list(lib))
  expect_null(x)

  processes <- replicate(num, simplify = FALSE,
    callr::r_bg(args = list(pkg, lib),
      function(pkg, lib) asNamespace("pkgdepends")$install_binary(pkg, lib = lib))
  )

  w <- lapply(processes, function(x) x$wait(5000))
  al <- vlapply(processes, function(x) x$is_alive())
  if (any(al)) stop("Some install processes did not finish")

  for (i in seq_len(num)) {
    expect_identical(processes[[i]]$get_result(), file.path(lib, "foo"))
  }
})

test_that("install_binary corrupt file", {
  pkg <- binary_test_package("foo")
  bin <- readBin(pkg, what = "raw", n = file.size(pkg))
  writeBin(head(bin, as.integer(length(bin)/2)), pkg)

  lib <- withr::local_tempdir()
  expect_snapshot(
    error = TRUE,
    install_binary(pkg, lib = lib, quiet = TRUE)
  )
})

test_that("install_binary errors", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("foobar\n", file = tmp)

  expect_error(
    install_binary(tmp, lib = tempdir(), quiet = TRUE),
    "unknown archive type", class = "install_input_error"
  )
})

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
    add_metadata(tmp, c(foo = "bar"))
  )
})

test_that("failing to move and/or remove existing installation", {
  # this is challenging to test, but we should do it, nevertheless
  skip("TODO")
})
