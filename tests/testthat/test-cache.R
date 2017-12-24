
context("cache")

test_that("init", {
  pc <- package_cache$new(tmp <- tempfile())
  expect_true(file.exists(tmp))
})

test_that("add / list / find / delete", {
  pc <- package_cache$new(tmp <- tempfile())
  cat("f1\n", file = f1 <- tempfile())

  md5 <- unname(tools::md5sum(f1))
  pc$add(f1, path = "f/b", package = "p", url = "u", etag = "e", md5 = md5)

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  expect_equal(
    as.list(pc$list()),
    list(fullpath = fullpath, path = path, package = "p", url = "u",
         etag = "e", md5 = md5)
  )

  expect_equal(
    as.list(pc$find(package = "p")),
    list(fullpath = fullpath, path = path, package = "p", url = "u",
         etag = "e", md5 = md5)
  )

  pc$copy_to(f2 <- tempfile(), package = "p")
  expect_true(file.exists(f2))
  expect_equal(unname(tools::md5sum(f2)), md5)

  pc$delete(package = "p")

  empty <- data.frame(
    stringsAsFactors = FALSE,
    fullpath = character(),  path = character(), package = character(),
    url = character(), etag = character(), md5 = character()
  )

  expect_equal(pc$list(), empty)

  pc$find(package = "p")
  expect_equal(pc$find(package = "p"), empty)
})

test_that("get_package_from", {

  skip_if_offline()
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  pc <- package_cache$new(tmp)

  urls <- c("https://eu.httpbin.org/etag/mytag")
  do <- async::async(function() {
    get_package_from(pc, urls, tmp2, file.path("foo", "bar", "out"))
  })
  res <- async::synchronise(do())

  expect_equal(res$status, "Got")
  expect_equal(res$url, urls)
  expect_equal(res$target, file.path(tmp2, "foo", "bar", "out"))
  expect_null(res$error)

  l <- pc$list()
  expect_equal(l$fullpath, file.path(tmp, "foo", "bar", "out"))
  expect_equal(l$path, file.path("foo", "bar", "out"))
  expect_equal(l$url, urls)
  expect_equal(l$etag, "mytag")
  expect_identical(l$package, NA_character_)
  expect_identical(l$md5, NA_character_)

  res2 <- async::synchronise(do())
  expect_equal(res2$status, "Had")
  expect_equal(res2$url, res$url)
  expect_equal(res2$target, res$target)
  expect_null(res2$error)

  l <- pc$list()
  expect_equal(l$fullpath, file.path(tmp, "foo", "bar", "out"))
  expect_equal(l$path, file.path("foo", "bar", "out"))
  expect_equal(l$url, urls)
  expect_equal(l$etag, "mytag")

  url2 <- c("https://eu.httpbin.org/status/404")
  do2 <- async::async(function() {
    get_package_from(pc, url2, tmp2, file.path("foo", "bar", "out"))
  })
  res3 <- async::synchronise(do2())

  expect_equal(res3$status, "Failed")
  expect_s3_class(
    res3$error,
    c("async_rejected", "download_try_list_error", "error")
  )
})

test_that("get_package_from, metadata", {

  skip_if_offline()
  skip_on_cran()

  tmp <- tempfile()
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  pc <- package_cache$new(tmp)

  urls <- c("https://eu.httpbin.org/etag/mytag")
  do <- async::async(function() {
    get_package_from(pc, urls, tmp2, file.path("foo", "bar", "out"),
                     metadata = list(package = "pkg", foo = "bar"))
  })
  res <- async::synchronise(do())

  expect_equal(res$status, "Got")
  expect_equal(res$url, urls)
  expect_equal(res$target, file.path(tmp2, "foo", "bar", "out"))
  expect_null(res$error)

  l <- pc$list()
  expect_equal(l$fullpath, file.path(tmp, "foo", "bar", "out"))
  expect_equal(l$path, file.path("foo", "bar", "out"))
  expect_equal(l$url, urls)
  expect_equal(l$etag, "mytag")
  expect_identical(l$package, "pkg")
  expect_identical(l$md5, NA_character_)
  expect_identical(l$foo, "bar")
})

test_that("cache dirs, on errors #1", {
  mockery::stub(
    detect_package_cache_dir,
    "get_user_cache_dir",
    function(...) stop("not available"))
  expect_warning(dir <- detect_package_cache_dir())
  expect_true(file.exists(dir))
  unlink(dir, recursive = TRUE)
})

test_that("cache dirs, on errors #2", {
  mockery::stub(
    detect_metadata_cache_dir,
    "user_metadata_cache_dir",
    function(...) stop("not available"))
  expect_warning(dir <- detect_metadata_cache_dir())
  expect_true(file.exists(dir))
  unlink(dir, recursive = TRUE)
})

test_that("update_metadata_cache warning on error", {
  mockery::stub(
    update_metadata_cache,
    "update_user_metadata_cache",
    function(...) stop("failed"))
  expect_warning(
    update_metadata_cache("foo", "bar"),
    "Cannot update metadata cache dir")
})
