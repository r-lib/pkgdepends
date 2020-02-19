
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
  expect_match(msg, "Getting 2 pkgs")

  what <- rbind(what, tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "installed",      10000, "foo",    "miss",
    "cran",           20000, "bar",    "hit"
  ))
  msg <- capture_async_messages(do())
  expect_match(msg, "Getting 2 pkgs")
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
    pkgplan__update_progress_bar(bar, 1L, "data", list(current = 5000, total = 10000))
    pkgplan__update_progress_bar(bar, 4L, "data", list(current = 20000, total = 20000))
    pkgplan__update_progress_bar(
      bar, 4L, "done",
      list(
        download_status = "Got",
        package = "pkg",
        platform = "source",
        version = "1.0.0",
        fulltarget = tempfile(),
        fulltarget_tree = tempfile()
      )
    )
    pkgplan__done_progress_bar(bar)
    bar$what
  }

  msg <- capture_async_messages(res <- do())
  expect_match(msg, "Getting 2 pkgs (30 kB)", fixed = TRUE)
  expect_match(crayon::strip_style(msg), "Got pkg 1.0.0 (source)", fixed = TRUE)
  expect_equal(res$filesize[1], 10000)
  expect_equal(res$filesize[4], 20000)
  expect_equal(res$current[1], 5000)
  expect_equal(res$current[4], 20000)
  expect_equal(res$status, c("data", "todo", "todo", "got"))
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
