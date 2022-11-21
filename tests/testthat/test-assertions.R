
test_that("is_character", {
  pos <- list("", "NA", "foobar", character(), letters, c(a = "b"))
  neg <- list(1, 1L, NA, NA_character_, c("x", NA_character_), NULL)
  for (p in pos) expect_true(is_character(p))
  for (n in neg) expect_false(is_character(n))
})

test_that("is_character errors", {
  asciicast::expect_snapshot_r_process(
    fn <- function(x) assert_that(is_character(x)),
    fn(1:2),
    fn(c("", NA_character_)),
    fn(rep(NA_character_, 5)),
    transform = function(x) {
      transform_no_srcref(transform_no_links(transform_show_cursor(x)))
    }
  )
})

test_that("is_character errors, noninteractive", {
  asciicast::expect_snapshot_r_process(
    interactive = FALSE,
    fn <- function(x) assert_that(is_character(x)),
    options(cli.unicode = FALSE),
    fn(1:2),
    transform = function(x) {
      transform_no_srcref(transform_no_links(transform_show_cursor(x)))
    }
  )
  asciicast::expect_snapshot_r_process(
    interactive = FALSE,
    fn <- function(x) assert_that(is_character(x)),
    options(cli.unicode = FALSE),
    fn(c("", NA_character_)),
    transform = function(x) {
      transform_no_srcref(transform_no_links(transform_show_cursor(x)))
    }
  )
  asciicast::expect_snapshot_r_process(
    interactive = FALSE,
    fn <- function(x) assert_that(is_character(x)),
    options(cli.unicode = FALSE),
    fn(rep(NA_character_, 5)),
    transform = function(x) {
      transform_no_srcref(transform_no_links(transform_show_cursor(x)))
    }
  )
})

test_that("is_string", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"))
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character(), NULL)
  for (p in pos) expect_true(is_string(p))
  for (n in neg) expect_false(is_string(n))
})

test_that("is_string errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_string(x)),
    fn(1:2),
    fn("foo"),
    fn(NA_character_),
    fn(NULL)
  )
})

test_that("is_optional_string", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"), NULL)
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character())
  for (p in pos) expect_true(is_optional_string(p))
  for (n in neg) expect_false(is_optional_string(n))
})

test_that("is_optional_string errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_optional_string(x)),
    fn(1:2),
    fn(c("foo", "bar")),
    fn(NA_character_)
  )
})

test_that("is_flag", {
  pos <- list(TRUE, FALSE)
  neg <- list(1, 1L, 1:10, NA, NA_character_)
  for (p in pos) expect_true(is_flag(p))
  for (n in neg) expect_false(is_flag(n))
})

test_that("is_flag errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_flag(x)),
    fn(NA),
    fn(1:10)
  )
})

test_that("is_path", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"))
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character(), NULL)
  for (p in pos) expect_true(is_path(p))
  for (n in neg) expect_false(is_path(n))
})

test_that("is_path errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_path(x)),
    fn(1:2),
    fn(c("foo", "bar")),
    fn(NA_character_)
  )
})

test_that("is_optional_path", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"), NULL)
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character())
  for (p in pos) expect_true(is_optional_path(p))
  for (n in neg) expect_false(is_optional_path(n))
})

test_that("is_optional path errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_optional_path(x)),
    fn(1:2),
    fn(c("foo", "bar")),
    fn(NA_character_)
  )
})

test_that("all_named", {
  pos <- list(character(), list(), c(a = "b"), c(a = 1, b = 2), NULL)
  neg <- list(1, 1L, 1:10, NA, c(a = 1, 2), list(a = 1, 1:5))
  for (p in pos) expect_true(all_named(p))
  for (n in neg) expect_false(all_named(n))
})

test_that("all_named errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(all_named(x)),
    fn(c(a = 1, 2)),
  )
})

test_that("is_existing_file", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_false(is_existing_file(tmp))
  expect_false(is_existing_file(file.path(tmp, "foo")))

  cat("foo\n", file = file.path(tmp, "foo"))
  expect_true(is_existing_file(file.path(tmp, "foo")))
})

test_that("is_existing_file errors", {
  asciicast::expect_snapshot_r_process(
    transform = function(x) transform_no_links(transform_show_cursor(x)),
    fn <- function(x) assert_that(is_existing_file(x)),
    fn("file1500454880b58"),
    fn(".")
  )
})

test_that("is_platform_list", {
  pos <- list("", "NA", "foobar", letters, c(a = "b"))
  neg <- list(1, 1L, NA, NA_character_, c("x", NA_character_), NULL,
              character())
  for (p in pos) expect_true(is_platform_list(p))
  for (n in neg) expect_false(is_platform_list(n))
})

test_that("is_platform_list errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_platform_list(x)),
    fn(1:10),
    fn(character()),
    fn(NA_character_)
  )
})

test_that("is_dependencies", {
  pos <- list(TRUE, FALSE, NA, NA_character_, character(),
              "Depends", c("Depends", "Imports"), pkg_dep_types(),
              list(direct = "all", indirect = "hard"))
  neg <- list(1, 1:5, "foo", c("Depends", NA), "linkingto",
              list(direct = "all", type = "all"))
  for (p in pos) expect_true(is_dependencies(p))
  for (n in neg) expect_false(is_dependencies(n))
})

test_that("is_dependencies errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_dependencies(x)),
    options(cli.unicode = FALSE),
    fn(c("Depends", NA))
  )
})

test_that("is_r_version_list", {
  pos <- list("1.2.3", c("3.2-2", "3.4"))
  neg <- list(character(), "foobar", NULL, 1, 1:5, c("1.4.5", NA),
              c("1.4.5", "foobar"))
  for (p in pos) expect_true(is_r_version_list(p), info = p)
  for (n in neg) expect_false(is_r_version_list(n), info = n)
})

test_that("is_r_version_list errors", {
  asciicast::expect_snapshot_r_process(
    transform = function(x) transform_show_cursor(transform_no_srcref(x)),
    fn <- function(x) assert_that(is_r_version_list(x)),
    fn(1:10),
    fn(character()),
    fn(c("1.2", "foo", "bar", "1.2.0"))
  )
})

test_that("is_difftime", {
  expect_true(is_difftime(as.difftime(1, units = "secs")))
  expect_false(is_difftime(100))
})

test_that("is_difftime errors", {
  asciicast::expect_snapshot_r_process(
    transform = transform_show_cursor,
    fn <- function(x) assert_that(is_difftime(x)),
    fn(1)
  )
})

test_that("is_count", {
  pos <- list(0, 0L, 1, 1L, 10000, 10000L)
  neg <- list(-1, 1.1, letters, integer(), 1:5, NA_integer_)

  for (p in pos) expect_true(is_count(p), info = p)
  for (n in neg) expect_false(is_count(n), info = n)

  expect_true(is_count(-10, min = -10))
  expect_true(is_count(-10, min = -100))
  expect_true(is_count(10, min = 10))
  expect_true(is_count(10, min = -100))

  expect_false(is_count(-10, min = -9))
  expect_false(is_count(-10, min = 0))
  expect_false(is_count(10, min = 11))
  expect_false(is_count(0, min = 1))
})

test_that("is_count errors", {
  fn <- function(x, min = 0) assert_that(is_count(x, min = min))
  expect_snapshot(error = TRUE, fn(letters))
  expect_snapshot(error = TRUE, fn(1:10))
  expect_snapshot(error = TRUE, fn(-1))
  expect_snapshot(error = TRUE, fn(0, min = 1))
  expect_snapshot(error = TRUE, fn(NA_integer_))
})
