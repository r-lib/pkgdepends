
context("cache api")

test_that("cache api", {
  dir.create(tmp <- tempfile())
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  ## This creates the cache
  sm <- cache_summary(tmp)
  expect_true(is.list(sm))
  expect_equal(sm$cachepath, tmp)
  expect_equal(sm$files, 0)
  expect_equal(sm$size, 0)
  expect_equal(nrow(cache_list(tmp)), 0)

  ## Add some files for testing
  pc <- package_cache$new(tmp)
  foo <- file.path(tmp2, "foo")
  cat("foo-contents\n", file = foo)

  bar <- file.path(tmp2, "bar")
  cat("bar-contents\n", file = bar)

  pc$add(foo, "dir/foo", package = "pkg1", version = "1.0.0")
  l <- cache_list(tmp)
  expect_equal(cache_summary(tmp)$files, 1)
  expect_equal(nrow(l), 1)

  pc$add(bar, "dir2/bar", package = "pkg2", version = "2.0.0")
  l <- cache_list(tmp)
  expect_equal(cache_summary(tmp)$files, 2)
  expect_equal(cache_summary(tmp)$size, sum(file.info(c(foo, bar))$size))
  expect_equal(nrow(l), 2)

  f1 <- cache_find(tmp, package = "pkg1")
  expect_equal(nrow(f1), 1)
  expect_equal(f1$package, "pkg1")
  expect_equal(f1$path, "dir/foo")

  expect_equal(nrow(cache_find(tmp, package = "blah")), 0)

  target <- file.path(tmp2, "target")
  cache_get_file(tmp, target = target, package = "pkg1")
  expect_true(file.exists(target))
  expect_equal(readLines(target), readLines(foo))

  expect_equal(
    nrow((cache_get_file(tmp, target = target, package = "blah"))),
    0)

  cache_delete_files(tmp, package = "pkg1")
  expect_equal(
    nrow((cache_get_file(tmp, target = target, package = "pkg1"))),
    0)

  l <- cache_list(tmp)
  expect_equal(cache_summary(tmp)$files, 1)
  expect_equal(cache_summary(tmp)$size, sum(file.info(bar)$size))
  expect_equal(nrow(l), 1)

  cache_delete_files(tmp)
  sm <- cache_summary(tmp)
  expect_equal(sm$cachepath, tmp)
  expect_equal(sm$files, 0)
  expect_equal(sm$size, 0)
  expect_equal(nrow(cache_list(tmp)), 0)
})
