
context("async http")

test_that("read_etag", {
  cat("foobar\n", file = tmp <- tempfile())
  expect_equal(read_etag(tmp), "foobar")

  cat("\"foobar\"", file = tmp)
  expect_equal(read_etag(tmp), "\"foobar\"")

  cat(" ", file = tmp)
  expect_equal(read_etag(tmp), " ")

  cat("", file = tmp)
  expect_true(length(tmp) == 1 && is.na(read_etag(tmp)))

  cat("foo\nbar", file = tmp)
  expect_equal(read_etag(tmp), "foo")

  expect_true(is.na(read_etag(tempfile())))
})

test_that("download_file", {

  skip_if_offline()

  dir.create(dir <- tempfile())
  dx <- synchronise(download_file(
    url    <- "https://httpbin.org/etag/foobar",
    target <- file.path(dir, "file1"),
    etag   <- file.path(dir, "etag")
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_file, errors", {

  skip_if_offline()

  tmp <- tempfile()
  expect_error(
    synchronise(download_file("http://0.42.42.42", tmp)),
    class = c("async_rejected", "async_http_error")
  )

  expect_error(
    synchronise(download_file("https://eu.httpbin.org/status/404", tmp)),
    class = c("async_rejected", "async_http_401", "async_http_error")
  )
})

test_that("download_if_newer, no etag file", {

  skip_if_offline()

  dir.create(dir <- tempfile())
  dx <- synchronise(download_if_newer(
    url    <- "https://httpbin.org/etag/foobar",
    target <- file.path(dir, "file1"),
    etag   <- file.path(dir, "etag")
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_if_newer, different etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("eeeetag\n", file = etag <- file.path(dir, "etag"))
  dx <- synchronise(download_if_newer(
    url    <- "https://httpbin.org/etag/foobar",
    target <- file.path(dir, "file1"),
    etag
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_if_newer, matching etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("foobar\n", file = etag <- file.path(dir, "etag"))
  cat("dummy\n", file = target <- file.path(dir, "file1"))
  dx <- synchronise(download_if_newer(
    url    <- "https://httpbin.org/etag/foobar",
    target,
    etag
  ))

  expect_true(file.exists(target))
  expect_equal(read_lines(target), "dummy")
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
  expect_equal(dx$response$status_code, 304)
})

test_that("download_if_newer, error", {

  skip_if_offline()

  cat("dummy\n", file = target <- tempfile())

  expect_error(
    synchronise(download_if_newer(
      url <- "http://0.42.42.42",
      destfile = target
    )),
    class = c("async_rejected", "async_http_error")
  )

  expect_error(
    synchronise(download_if_newer(
      url <- "https://httpbin.org/status/404",
      destfile = target
    )),
    class = c("async_rejected", "async_http_404", "async_http_error")
  )
})

test_that("download_one_of", {

  skip_if_offline()

  dx <- synchronise(download_one_of(
    c("https://httpbin.org/status/404",
      "https://httpbin.org/status/403",
      "https://httpbin.org/get?q=1"),
    tmp <- tempfile()
  ))

  res <- jsonlite::fromJSON(read_lines(tmp), simplifyVector = FALSE)
  expect_equal(res$args$q, "1")
})

test_that("download_one_of, etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("eeeetag\n", file = etag <- file.path(dir, "etag"))
  dx <- synchronise(download_one_of(
    c("https://httpbin.org/status/404",
      "https://httpbin.org/status/403",
      url <- "https://httpbin.org/etag/foobar"),
    target <- file.path(dir, "file1"),
    etag_file = etag
  ))

  expect_true(file.exists(target))
  expect_equal(jsonlite::fromJSON(target)$url, url)
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_one_of, matching etag", {

  skip_if_offline()

  dir.create(dir <- tempfile())

  cat("foobar\n", file = etag <- file.path(dir, "etag"))
  cat("dummy\n", file = target <- file.path(dir, "file1"))
  dx <- synchronise(download_one_of(
    c("https://httpbin.org/status/404",
      "https://httpbin.org/status/403",
      url <- "https://httpbin.org/etag/foobar"),
    target,
    etag_file = etag
  ))

  expect_true(file.exists(target))
  expect_equal(read_lines(target), "dummy")
  expect_true(file.exists(etag))
  expect_equal(read_lines(etag), "foobar")
})

test_that("download_one_of, errors", {

  skip_if_offline()

  tmp <- tempfile()

  afun <- async(function() {
    download_one_of(
      c("https://httpbin.org/status/404",
        "https://httpbin.org/status/403",
        "https://httpbin.org/status/404"),
      tmp
    )
  })

  err <- tryCatch(synchronise(afun()), error = identity)
  expect_match(conditionMessage(err), "All URLs failed")
  expect_true("download_one_of_error" %in% class(err))
  expect_false(file.exists(tmp))
})
