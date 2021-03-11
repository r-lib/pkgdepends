
test_that("install_binary", {

  pkg <- binary_test_package("foo")
  libpath <- test_temp_dir()

  suppressMessages(
    install_binary(pkg, lib = libpath, quiet = TRUE)
  )
  x <- callr::r(function(l) {
    library("foo", lib.loc = l)
    foo::foo()
  }, list(libpath))
  expect_null(x)
})

test_that("install_binary works for simultaneous installs", {
  skip_on_cran()

  pkg <- binary_test_package("foo")
  libpath <- test_temp_dir()

  processes <- list()
  num <- 5

  # install and load foo here to test loaded DLLs in another process
  suppressMessages(
    install_binary(pkg, lib = libpath, quiet = TRUE)
  )
  x <- callr::r(function(l) {
    library("foo", lib.loc = l)
    foo::foo()
  }, list(libpath))
  expect_null(x)

  processes <- replicate(num, simplify = FALSE,
    callr::r_bg(args = list(pkg, libpath),
      function(pkg, libpath) asNamespace("pkgdepends")$install_binary(pkg, lib = libpath))
  )

  w <- lapply(processes, function(x) x$wait(5000))
  al <- vlapply(processes, function(x) x$is_alive())
  if (any(al)) stop("Some install processes did not finish")

  for (i in seq_len(num)) {
    expect_identical(processes[[i]]$get_result(), file.path(libpath, "foo"))
  }
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
