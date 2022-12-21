
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
  expect_error(
    git_list_files(
      "https://github.com/gaborcsardi/pak-test.git",
      "foobar"
    ),
    "Unknown ref"
  )
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

})

test_that("raw_as_utf8", {

})

test_that("git_parse_message", {

})

test_that("git_create_message_v2", {

})

test_that("git_send_message", {

})

test_that("pkt_line", {

})

test_that("git_list_refs_v1", {

})

test_that("git_parse_okt_line_refs", {

})

test_that("git_list_refs_v2", {

})

test_that("git_unpack", {
  path <- test_path("fixtures/git-test-1.pack")
  up1 <- git_unpack(path)
  up2 <- git_unpack(readBin(path, "raw", file.size(path)))
  expect_equal(up1, up2)
  expect_snapshot(git_unpack(path))
})

test_that("parse_int32_nwb", {

})

test_that("parse_size", {

})

test_that("parse_tree", {

})

test_that("bin_to_sha", {

})

test_that("parse_commit", {

})
