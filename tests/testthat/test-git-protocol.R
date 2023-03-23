
test_that("git_list_refs", {
  skip_on_cran()
  expect_snapshot(
    git_list_refs("https://github.com/gaborcsardi/pak-test.git")$refs
  )
  expect_snapshot(
    git_list_refs(
      "https://github.com/gaborcsardi/pak-test.git",
      "refs/heads/"
    )$refs
  )
})

test_that("git_list_files", {
  skip_on_cran()
  expect_error(
    git_list_files(
      "https://github.com/gaborcsardi/pak-test.git",
      "foobar"
    ),
    "Unknown git ref"
  )
  if (!l10n_info()[["UTF-8"]]) skip("UTF-8 snapshot")
  expect_snapshot({
    git_list_files(
      "https://github.com/gaborcsardi/pak-test.git",
      "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
    )
    git_list_files(
      "https://github.com/gaborcsardi/pak-test.git",
      "refs/tags/v1"
    )
  })
})

test_that("git_download_file", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  expect_snapshot({
    out <- git_download_file(
      "https://github.com/gaborcsardi/pak-test.git",
      "a1e2d6741374d1f32ec138ee2020eae36b859e99",
      tmp
    )
    out
    readLines(tmp)
  })
})

test_that("git_fetch", {
  # TODO
  expect_true(TRUE)
})

test_that("raw_as_utf8", {
  # TODO
  expect_true(TRUE)
})

test_that("git_parse_message", {
  # TODO
  expect_true(TRUE)
})

test_that("git_create_message_v2", {
  # TODO
  expect_true(TRUE)
})

test_that("git_send_message", {
  # TODO
  expect_true(TRUE)
})

test_that("pkt_line", {
  # TODO
  expect_true(TRUE)
})

test_that("git_list_refs_v1", {
  # TODO
  expect_true(TRUE)
})

test_that("git_parse_okt_line_refs", {
  # TODO
  expect_true(TRUE)
})

test_that("git_list_refs_v2", {
  # TODO
  expect_true(TRUE)
})

test_that("git_unpack", {
  path <- test_path("fixtures/git-test-1.pack")
  up1 <- git_unpack(path)
  up2 <- git_unpack(readBin(path, "raw", file.size(path)))
  expect_equal(up1, up2)
  if (!l10n_info()[["UTF-8"]]) skip("UTF-8 snapshot")
  expect_snapshot(git_unpack(path))
})

test_that("parse_int32_nwb", {
  # TODO
  expect_true(TRUE)
})

test_that("parse_size", {
  # TODO
  expect_true(TRUE)
})

test_that("parse_tree", {
  # TODO
  expect_true(TRUE)
})

test_that("bin_to_sha", {
  # TODO
  expect_true(TRUE)
})

test_that("parse_commit", {
  # TODO
  expect_true(TRUE)
})
