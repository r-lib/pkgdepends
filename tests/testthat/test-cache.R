
context("cache")

test_that("init", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))
  expect_true(file.exists(tmp))
})

test_that("add / list / find / delete", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))
  cat("f1\n", file = f1 <- tempfile())

  md5 <- unname(tools::md5sum(f1))
  new <- pc$add(f1, path = "f/b", package = "p", url = "u",
                etag = "e", md5 = md5)

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = "u",
              etag = "e", md5 = md5)

  expect_equal(as.list(new), exp)

  expect_equal(as.list(pc$list()), exp)

  expect_equal(as.list(pc$find(package = "p")), exp)

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

test_that("add_url", {

  skip_if_offline()

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  url <- "http://httpbin.org/etag/foobar"
  new <- pc$add_url(url, "f/b", package = "p")

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, url = url, etag = "foobar",
              package = "p", md5 = md5sum(fullpath)[[1]])
  expect_equal(as.list(new), exp)
})

test_that("copy_or_add, positive", {
  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  md5 <- unname(tools::md5sum(f1))
  new <- pc$add(f1, path = "f/b", package = "p", url = "u",
                etag = "e", md5 = md5)
  attr(new, "action") <- "Had"

  hit <- pc$copy_or_add(f1 <- tempfile(), url = "u", path = "f/b",
                        package = "p")
  expect_true(file.exists(f1))
  expect_equal(readLines(f1), "f1")
  expect_equal(new, hit)
})

test_that("copy_or_add, negative", {

  skip_if_offline()

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  md5 <- unname(tools::md5sum(f1))
  new <- pc$add(f1, path = "f/b", package = "p", url = "u",
                etag = "e", md5 = md5)

  url <- "http://httpbin.org/etag/foobar"
  hit <- pc$copy_or_add(url = url, f1 <- tempfile(), path = "f/b",
                        package = "p2")

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p2", url = url,
              etag = "foobar", md5 = md5sum(fullpath)[[1]])
  attr(exp, "action") <- "Got"
  expect_equal(as.list(hit), exp)
  expect_true(file.exists(f1))
  expect_true(any(grepl("url\"*:.*http://httpbin.org/etag/foobar",
                        readLines(f1))))

  hit2 <- pc$find(url = url)
  attr(hit2, "action") <- "Got"
  expect_equal(hit2, hit)
})

test_that("update_or_add, not in cache", {

  skip_if_offline()

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  url <- "http://httpbin.org/etag/foobar"
  hit <- pc$update_or_add(url = url, f1 <- tempfile(), path = "f/b",
                          package = "p")

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, url = url, etag = "foobar",
              package = "p", md5 = md5sum(fullpath)[[1]])
  attr(exp, "action") <- "Got"
  expect_equal(as.list(hit), exp)

  expect_true(file.exists(f1))
  expect_true(any(grepl("url\"*:.*http://httpbin.org/etag/foobar",
                        readLines(f1))))

  hit2 <- pc$find(url = url)
  attr(hit2, "action") <- "Got"
  expect_equal(hit2, hit)
})

test_that("update_or_add, cache is too old", {
  skip_if_offline()

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  url <- "http://httpbin.org/etag/foobar"
  md5 <- unname(tools::md5sum(f1))
  pc$add(f1, path = "f/b", package = "p", url = url, etag = "e", md5 = md5)

  hit <- pc$update_or_add(url = url, f1 <- tempfile(), path = "f/b",
                          package = "p")

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = url,
              etag = "foobar", md5 = md5sum(fullpath)[[1]])
  attr(exp, "action") <- "Got"
  expect_equal(as.list(hit), exp)

  expect_true(file.exists(f1))
  expect_true(any(grepl("url\"*:.*http://httpbin.org/etag/foobar",
                        readLines(f1))))

  hit2 <- pc$find(url = url, etag = "foobar")
  attr(hit2, "action") <- "Got"
  expect_equal(hit2, hit)
})

test_that("update_or_add, cache is current", {
  skip_if_offline()

  pc <- package_cache$new(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  cat("f1\n", file = f1 <- tempfile())

  url <- "http://httpbin.org/etag/foobar"
  md5 <- unname(tools::md5sum(f1))
  pc$add(f1, path = "f/b", package = "p", url = url, etag = "foobar",
         md5 = md5)

  hit <- pc$update_or_add(url = url, f1 <- tempfile(), path = "f/b",
                          package = "p")

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  exp <- list(fullpath = fullpath, path = path, package = "p", url = url,
              etag = "foobar", md5 = md5)
  attr(exp, "action") <- "Current"
  expect_equal(as.list(hit), exp)

  expect_true(file.exists(f1))
  expect_equal(readLines(f1), "f1")

  hit2 <- pc$find(url = url)
  attr(hit2, "action") <- "Current"
  expect_equal(hit2, hit)
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
    c("async_rejected", "download_one_of_error", "error")
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
