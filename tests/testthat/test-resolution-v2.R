
context("resolution")

test_that("resolving with a list", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  do <- function() {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes("foo::bar"))
    res$push(.list = parse_remotes("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$rest,
         version = "1.0.0", sources = c("src1", "src2"))
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 2)
  expect_identical(res$ref, c("foo::bar", "foo::bar2"))
  expect_identical(res$package, c("bar", "bar2"))
  expect_identical(res$status, c("OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(res$sources, list(c("src1", "src2"), c("src1", "src2")))
})

test_that("resolving with a tibble", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  do <- function() {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes("foo::bar"))
    res$push(.list = parse_remotes("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    tibble(ref = c("ref1", "ref2"), type = c("type1", "type2"),
           package = c("pkg1", "pkg2"), version = c("ver1", "ver2"),
           sources = list(c("s11", "s12"), c("s21", "s22")))
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 4)
  expect_identical(res$ref, c("ref1", "ref2", "ref1", "ref2"))
  expect_identical(res$package, c("pkg1", "pkg2", "pkg1", "pkg2"))
  expect_identical(res$status, c("OK", "OK", "OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(
    res$sources,
    list(c("s11", "s12"), c("s21", "s22"), c("s11", "s12"), c("s21", "s22"))
  )
})

test_that("unknown deps are pushed in the queue", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  do <- function() {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes("foo::bar"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$rest,
         version = "1.0.0", sources = c("src1", "src2"),
         unknown_deps = "foo::bar2")
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 2)
  expect_identical(res$ref, c("foo::bar", "foo::bar2"))
  expect_identical(res$package, c("bar", "bar2"))
  expect_identical(res$status, c("OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(res$sources, list(c("src1", "src2"), c("src1", "src2")))
})

test_that("unknown deps, tibble", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  do <- function() {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes("foo::bar"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    tibble(ref = c("ref1", "ref2"), type = c("type1", "type2"),
           package = c("pkg1", "pkg2"), version = c("ver1", "ver2"),
           sources = list(c("s11", "s12"), c("s21", "s22")),
           unknown_deps = "foo::bar2")
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 4)
  expect_identical(res$ref, c("ref1", "ref2", "ref1", "ref2"))
  expect_identical(res$package, c("pkg1", "pkg2", "pkg1", "pkg2"))
  expect_identical(res$status, c("OK", "OK", "OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(
    res$sources,
    list(c("s11", "s12"), c("s21", "s22"), c("s11", "s12"), c("s21", "s22"))
  )
})

test_that("error", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  do <- function() {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes("foo::bar"))
    res$push(.list = parse_remotes("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    stop("foobar", call. = FALSE)
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 2)
  expect_equal(res$ref, c("foo::bar", "foo::bar2"))
  expect_s3_class(res$error[[1]], "error")
  expect_s3_class(res$error[[2]], "error")
  expect_equal(conditionMessage(res$error[[1]]), "foobar")
  expect_equal(conditionMessage(res$error[[2]]), "foobar")
  expect_equal(res$status, c("FAILED", "FAILED"))
  expect_equal(res$type, c("foo", "foo"))
})

test_that("installed refs are also resolved", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  mkdirp(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  mkdirp(file.path(lib, "bar"))
  mkdirp(file.path(lib, "bar2"))

  do <- function() {
    res <- resolution$new(config = conf, cache = cache, library = lib)
    res$push(.list = parse_remotes("foo::bar"))
    res$push(.list = parse_remotes("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$rest,
         version = "1.0.0", sources = c("src1", "src2"))
  }

  inst_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$package,
         version = "1.0.9", sources = c("i1", "i"))
  }

  types <- list(
    foo = list(resolve = foo_resolve),
    installed = list(resolve = inst_resolve))

  res <- withr::with_options(
    list(pkg.remote_types = types),
    synchronise(do()))

  expect_equal(nrow(res), 4)
  expect_equal(res$ref[1:2], c("foo::bar", "foo::bar2"))
  lib <- normalizePath(lib, winslash = "/", mustWork = TRUE)
  expect_equal(res$ref[3:4], paste0("installed::", lib, "/", c("bar", "bar2")))
  expect_equal(res$status, rep("OK", 4))
  expect_equal(res$package, c("bar", "bar2", "bar", "bar2"))
})
