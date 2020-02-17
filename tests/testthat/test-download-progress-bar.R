
test_that("make_bar", {
  # TODO
  expect_true(TRUE)
})

test_that("initial state", {
  do <- function() pkgplan__create_progress_bar(what = what)

  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "cran",           20000, "bar",    "miss"
  )
  msg <- capture_async_messages(do())
  expect_match(msg, "About to download 2 packages")

  what <- rbind(what, tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "installed",      10000, "foo",    "miss",
    "cran",           20000, "bar",    "hit"
  ))
  msg <- capture_async_messages(do())
  expect_match(msg, "About to download 2 packages")
})

test_that("updates", {
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "installed",      10000, "foo",    "miss",    # no download, installed
    "cran",           20000, "bar",    "hit",     # no download, cached
    "cran",           20000, "bar",    "miss"
  )

  do <- function() {
    bar <- pkgplan__create_progress_bar(what = what)
    pkgplan__update_progress_bar(bar, 1L, list(current = 5000, total = 10000))
    pkgplan__update_progress_bar(bar, 4L, list(current = 20000, total = 20000))
    pkgplan__done_progress_bar(bar)
    bar$what
  }

  msg <- capture_async_messages(res <- do())
  expect_equal(res$filesize[1], 10000)
  expect_equal(res$filesize[4], 20000)
  expect_equal(res$current[1], 5000)
  expect_equal(res$current[4], 20000)
  expect_equal(res$event, c("got", "todo", "todo", "done"))
  expect_s3_class(res$event_at, "POSIXct")
})

test_that("rate is calculated properly", {
  # TODO
  expect_true(TRUE)
})

test_that("eta is calculated properly", {
  # TODO
  expect_true(TRUE)
})

test_that("if file size is unknown", {
  # TODO
  expect_true(TRUE)
})

test_that("status bar is cleaned up on interrupt / error", {
  # TODO
  expect_true(TRUE)
})
