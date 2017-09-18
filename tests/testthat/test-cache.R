
context("cache")

test_that("init", {
  pc <- package_cache$new(tmp <- tempfile())
  expect_true(file.exists(tmp))
})

test_that("add / list / find / delete", {
  pc <- package_cache$new(tmp <- tempfile())
  cat("f1\n", file = f1 <- tempfile())

  pc$add(f1, path = "f/b", package = "p", url = "u", etag = "e", md5 = "5")

  path <- file.path("f", "b")
  fullpath <- file.path(tmp, path)
  expect_equal(
    as.list(pc$list()),
    list(fullpath = fullpath, path = path, package = "p", url = "u",
         etag = "e", md5 = "5")
  )

  expect_equal(
    as.list(pc$find(package = "p")),
    list(fullpath = fullpath, path = path, package = "p", url = "u",
         etag = "e", md5 = "5")
  )

  pc$delete(package = "p")
  expect_equal(
    pc$list(),
    make_empty_db_data_frame()
  )

  pc$find(package = "p")
  expect_equal(
    pc$find(package = "p"),
    make_empty_db_data_frame()
  )
})
