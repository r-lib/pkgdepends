
test_that("parse_remote_deps", {
  expect_snapshot(parse_remote_deps("deps::/foo/bar"))
  expect_snapshot(parse_remote_deps("deps::foo/bar"))
  expect_snapshot(parse_remote_deps("deps::~foo/bar"))
})

test_that("resolve_remote_deps", {
  setup_fake_apps()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  writeLines(
    c("Package: mypackage",
      "Version: 1.0.0",
      "Depends: pkg2",
      "Imports: pkg3",
      "Suggests: needspak"),
    "DESCRIPTION"
  )

  prop <- suppressMessages(new_pkg_installation_proposal(
    "deps::.",
    config = list(library = tempfile())
  ))
  suppressMessages(prop$resolve())
  expect_snapshot(
    prop$get_resolution()[, c("ref", "package", "version")]
  )
  prop$solve()
  expect_snapshot(
    prop$get_solution()
  )
})

test_that("download_remote_deps", {
  expect_equal(download_remote_deps(), "Had")
})

test_that("satisfy_remote_seps", {
  expect_false(satisfy_remote_deps())
})

test_that("installedok_remote_deps", {
  expect_false(installedok_remote_deps())
})

test_that("type_deps_rx", {
  expect_true(grepl(type_deps_rx(), "deps::/foo/bar", perl = TRUE))
  expect_true(grepl(type_deps_rx(), "deps::foo/bar", perl = TRUE))
  expect_true(grepl(type_deps_rx(), "deps::~/foo/bar", perl = TRUE))
})
