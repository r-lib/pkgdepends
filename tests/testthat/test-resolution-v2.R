
context("resolution")

test_that("resolving with a list", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = NULL)
  do <-  function() {
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
  do <-  function() {
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
  do <-  function() {
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
  do <-  function() {
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
