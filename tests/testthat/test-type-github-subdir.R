test_that("github_subdir_candidates lists root first", {
  expect_identical(github_subdir_candidates(), c("", "pkg-r", "r", "R"))
})

test_that("github_query_subdirs uses supplied subdir or candidates", {
  expect_identical(github_query_subdirs(list(subdir = "foo")), "foo")
  expect_identical(github_query_subdirs(list(subdir = "")), github_subdir_candidates())
  expect_identical(github_query_subdirs(list()), github_subdir_candidates())
})

test_that("github_subdir_aliases are descN in order", {
  expect_identical(github_subdir_aliases(3), c("desc1", "desc2", "desc3"))
  expect_identical(github_subdir_aliases(0), character())
})

test_that("github_subdir_paths appends a slash to non-root dirs", {
  expect_identical(
    github_subdir_paths(c("", "pkg-r", "r")),
    c("", "pkg-r/", "r/")
  )
  expect_identical(github_subdir_paths(character(0)), character(0))
})

test_that("github_ref_desc_fragment builds one aliased object per dir", {
  frag <- github_ref_desc_fragment("HEAD", c("", "pkg-r"))
  expect_match(frag, "desc1: object\\(expression: \"HEAD:DESCRIPTION\"\\)", fixed = FALSE)
  expect_match(frag, "desc2: object\\(expression: \"HEAD:pkg-r/DESCRIPTION\"\\)", fixed = FALSE)
})

test_that("github_file_desc_fragment builds one aliased file per dir", {
  frag <- github_file_desc_fragment(c("", "pkg-r"))
  expect_match(frag, "desc1: file\\(path: \"DESCRIPTION\"\\)", fixed = FALSE)
  expect_match(frag, "desc2: file\\(path: \"pkg-r/DESCRIPTION\"\\)", fixed = FALSE)
})

test_that("github_pick_desc returns first non-null hit in order", {
  obj <- list(d = list(
    desc1 = NULL,
    desc2 = list(isBinary = FALSE, text = "Package: foo\n")
  ))
  get_node <- function(o, a) o$d[[a]]
  hit <- github_pick_desc(obj, c("", "pkg-r"), get_node, rem = list(), call. = NULL)
  expect_identical(hit, list(text = "Package: foo\n", subdir = "pkg-r"))
})

test_that("github_pick_desc skips nodes that exist but have no text", {
  obj <- list(d = list(
    desc1 = list(isBinary = FALSE, text = NULL),
    desc2 = list(isBinary = FALSE, text = "Package: foo\n")
  ))
  get_node <- function(o, a) o$d[[a]]
  hit <- github_pick_desc(obj, c("", "pkg-r"), get_node, rem = list(), call. = NULL)
  expect_identical(hit$subdir, "pkg-r")
})

test_that("github_pick_desc returns NULL when no candidate matches", {
  obj <- list(d = list(desc1 = NULL, desc2 = NULL))
  get_node <- function(o, a) o$d[[a]]
  expect_null(github_pick_desc(obj, c("", "pkg-r"), get_node, rem = list(), call. = NULL))
})

test_that("github_pick_desc throws baddesc when the first hit is binary", {
  obj <- list(d = list(desc1 = list(isBinary = TRUE, text = NULL)))
  get_node <- function(o, a) o$d[[a]]
  rem <- list(username = "u", repo = "r", subdir = "")
  expect_error(
    github_pick_desc(obj, "", get_node, rem = rem, call. = NULL),
    "Can't parse DESCRIPTION"
  )
})

test_that("ref resolution auto-detects pkg-r/ subdir", {
  setup_fake_gh_app()
  r <- pkg_plan$new(
    "r-lib/subdirpkg",
    config = list(library = tempfile(), dependencies = FALSE)
  )
  suppressMessages(r$resolve())
  res <- r$get_resolution()

  expect_true(all(res$status == "OK"))
  expect_identical(res$package, "subdirpkg")
  expect_identical(res$metadata[[1]][["RemoteSubdir"]], "pkg-r")
})

test_that("ref resolution prefers r/ over R/", {
  setup_fake_gh_app()
  r <- pkg_plan$new(
    "r-lib/subdirprio",
    config = list(library = tempfile(), dependencies = FALSE)
  )
  suppressMessages(r$resolve())
  res <- r$get_resolution()

  expect_true(all(res$status == "OK"))
  expect_identical(res$package, "subdirprio")
  expect_identical(res$metadata[[1]][["RemoteSubdir"]], "r")
})

test_that("ref resolution with root DESCRIPTION sets no RemoteSubdir", {
  setup_fake_gh_app()
  r <- pkg_plan$new(
    "r-lib/crayon",
    config = list(library = tempfile(), dependencies = FALSE)
  )
  suppressMessages(r$resolve())
  res <- r$get_resolution()

  expect_true(all(res$status == "OK"))
  expect_identical(res$package, "crayon")
  expect_false("RemoteSubdir" %in% names(res$metadata[[1]]))
})

test_that("ref resolution fails when no candidate has a package", {
  setup_fake_gh_app()
  r <- pkg_plan$new(
    "tidyverse/tidyverse.org",
    config = list(library = tempfile(), dependencies = FALSE)
  )
  suppressMessages(r$resolve())
  res <- r$get_resolution()
  expect_true(all(res$status == "FAILED"))
})

test_that("explicit subdir is queried alone, no probing fallback", {
  setup_fake_gh_app()
  # crayon has a root DESCRIPTION but nothing in nonexistent/
  r <- pkg_plan$new(
    "r-lib/crayon/nonexistent",
    config = list(library = tempfile(), dependencies = FALSE)
  )
  suppressMessages(r$resolve())
  res <- r$get_resolution()
  expect_true(all(res$status == "FAILED"))
})

test_that("pull request resolution auto-detects pkg-r/ subdir", {
  setup_fake_gh_app()
  r <- pkg_plan$new(
    "r-lib/subdirpkg#12",
    config = list(library = tempfile(), dependencies = FALSE)
  )
  suppressMessages(r$resolve())
  res <- r$get_resolution()

  expect_true(all(res$status == "OK"))
  expect_identical(res$package, "subdirpkg")
  expect_identical(res$metadata[[1]][["RemoteSubdir"]], "pkg-r")
})
