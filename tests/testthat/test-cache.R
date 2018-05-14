
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

test_that("cache dirs, on errors #1", {
  mockery::stub(
    detect_package_cache_dir,
    "get_user_cache_dir",
    function(...) stop("not available"))
  expect_warning(dir <- detect_package_cache_dir())
  expect_true(file.exists(dir))
  unlink(dir, recursive = TRUE)
})
