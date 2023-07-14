
test_that("raw_as_utf8", {
  # TODO
  expect_true(TRUE)
})

test_that("git_parse_message errors", {
  expect_snapshot(
    error = TRUE,
    git_parse_message(raw(3))
  )
  expect_snapshot(
    error = TRUE,
    git_parse_message(charToRaw("foobvar"))
  )
  expect_snapshot(
    error = TRUE,
    git_parse_message(charToRaw("00bbnoteonugh"))
  )
})

test_that("git_create_message_v1", {
  expect_snapshot(
    error = TRUE,
    git_create_message_v1(character())
  )
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
  expect_snapshot(
    error = TRUE,
    pkt_line(raw(70000))
  )
})

test_that("async_git_list_refs_v2_process_1", {
  # works with a v1 response as well
  resp <- readRDS(test_path("fixtures", "git-response-v1.rds"))
  expect_snapshot(
    sy(async_git_list_refs_v2_process_1(
      resp,
      "https://github.com/gaborcsardi/pak-test.git",
      "refs/tags/v1"
    ))$refs
  )
})

test_that("async_git_list_refs_v2_process_2", {
  psd <- readRDS(test_path("fixtures", "git-response-parsed-v2.rds"))
  url <- "http://localhost:3000/git/cli"
  psd2 <- psd
  psd2[[1]]$text <- NULL
  expect_snapshot(
    error = TRUE,
    async_git_list_refs_v2_process_2(NULL, psd2, url, NULL)
  )

  psd2 <- psd
  psd2[[1]]$text <- "version 10"
  expect_snapshot(
    error = TRUE,
    async_git_list_refs_v2_process_2(NULL, psd2, url, NULL)
  )

  psd2 <- psd
  psd2[[length(psd2)]]$type <- "data-pkt"
  expect_snapshot(
    error = TRUE,
    async_git_list_refs_v2_process_2(NULL, psd2, url, NULL)
  )
})

test_that("check_initial_response", {
  expect_snapshot(
    error = TRUE,
    check_initial_response(list(), "http://localhost:3000/git/cli")
  )
})

test_that("async_git_list_refs_v2_process_2", {
  url <- "http://localhost:3000/git/cli"
  expect_snapshot(
    error = TRUE,
    async_git_list_refs_v2_process_3(
      list(list(type = "data-pkt")),
      character(),
      url
    )
  )

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

test_that("git_list_pack_index", {
  path <- test_path("fixtures/git-test-1.idx")
  expect_snapshot(git_list_pack_index(path))
})

test_that("git_unpack errors", {
  path <- test_path("fixtures/git-test-1.pack")
  pack <- readBin(path, "raw", file.size(path))
  expect_snapshot(
    error = TRUE,
    git_unpack(pack[1:30])
  )

  expect_snapshot(
    error = TRUE,
    git_unpack(charToRaw("nope and some more so we have enough bytes"))
  )

  pack2 <- pack
  pack2[8] <- as.raw(1)
  expect_snapshot(
    error = TRUE,
    git_unpack(pack2)
  )

  pack2 <- pack
  pack2[length(pack2)] <- as.raw(11)
  expect_snapshot(
    error = TRUE,
    git_unpack(pack2)
  )
})

test_that("parse_int32_nwb", {
  expect_snapshot(
    error = TRUE,
    parse_int32_nwb(raw(3))
  )
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

test_that("git_fetch_process_v1", {
  url <- "https://user:secret@example.com"
  expect_snapshot(
    error = TRUE,
    git_fetch_process_v1(list(), url, "badcafe")
  )

  expect_snapshot(
    error = TRUE,
    git_fetch_process_v1(list(list(type = "boo")), url, "badcafe")
  )
})
