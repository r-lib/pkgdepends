
withr::local_envvar(GITHUB_PAT="FAIL")

test_that("git_list_refs", {
  skip_on_cran()
  expect_snapshot(
    git_list_refs(fake_git$url("/pak-test.git"))$refs
  )

  # filter
  expect_snapshot(
    git_list_refs_v2(
      fake_git$url("/pak-test.git"),
      "refs/heads/"
    )$refs
  )
})

test_that("git_list_files", {
  skip_on_cran()
  expect_error(
    git_list_files(
      fake_git$url("/pak-test.git"),
      "foobar"
    ),
    "Unknown git ref"
  )
  if (!l10n_info()[["UTF-8"]]) skip("UTF-8 snapshot")
  expect_snapshot({
    git_list_files(
      fake_git$url("/pak-test.git"),
      "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
    )
    git_list_files(
      fake_git$url("/pak-test.git"),
      "refs/tags/v1"
    )
  })
})

test_that("async_git_list_files_process", {
  # reordering objects in a PACK file does not matter
  ref <- "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
  pack <- git_fetch(fake_git$url("/pak-test.git"), ref, blobs = FALSE)

  withr::local_seed(13L)
  pack <- sample(pack)

  expect_snapshot(
    sort(async_git_list_files_process(
      pack,
      ref = ref,
      sha = ref,
      url = "url"
    )$files$path)
  )
})

test_that("git_download_file", {
  skip_on_cran()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  expect_snapshot({
    out <- git_download_file(
      fake_git$url("/pak-test.git"),
      "a1e2d6741374d1f32ec138ee2020eae36b859e99",
      tmp
    )
    out
    readLines(tmp)
  })
})

test_that("git_fetch", {
  skip_on_cran()

  # force v1
  pack <- git_fetch(
    fake_git$url("/pak-test.git"),
    structure("cefdc0eebcd7f757efb9a80652fd8aaf1a87508e", protocol = "1")
  )

  if (!l10n_info()[["UTF-8"]]) skip("UTF-8 snapshot")
  expect_snapshot(cat(pack[[1]]$object))
})

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

test_that("git_list_refs_v1", {
  skip_on_cran()
  expect_snapshot(
    git_list_refs_v1(fake_git$url("/pak-test.git"))$refs
  )
})

test_that("git_list_refs_v1_process_1", {
  skip_on_cran()
  resp <- readRDS(test_path("fixtures", "git-response-v1.rds"))
  expect_snapshot(
    git_list_refs_v1_process_1(
      resp,
      fake_git$url("/pak-test.git"),
      "refs/tags/v1"
    )$refs
  )
})

test_that("async_git_list_refs_v2_process_1", {
  skip_on_cran()
  # works with a v1 response as well
  resp <- readRDS(test_path("fixtures", "git-response-v1.rds"))
  expect_snapshot(
    sy(async_git_list_refs_v2_process_1(
      resp,
      fake_git$url("/pak-test.git"),
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

test_that("async_git_resolve_ref", {
  skip_on_cran()

  # branches w/o refs/heads/ prefix
  expect_snapshot(
    sy(async_git_resolve_ref(
      fake_git$url("/pak-test.git"),
      "main"
    ))
  )

  # tags w/o refs/rags/ prefix
  expect_snapshot(
    sy(async_git_resolve_ref(
      fake_git$url("/pak-test.git"),
      "v1"
    ))
  )

  # sha prefix
  expect_snapshot(
    sy(async_git_resolve_ref(
      fake_git$url("/pak-test.git"),
      "3f3b0b4ee8a0ff"
    ))
  )

  # unknown sha prefix -> error
  expect_snapshot(
    error = TRUE,
    sy(async_git_resolve_ref(
      fake_git$url("/pak-test.git"),
      "badcafe"
    ))
  )

  mockery::stub(
    async_git_resolve_ref, "async_git_list_refs",
    function(...) async_constant(list(
      refs = data_frame(
        ref = c("one", "two"),
        hash = c("badcafe1", "badcafe2")
      ),
      caps = character()
    ))
  )

  expect_snapshot(
    error = TRUE,
    sy(async_git_resolve_ref(
      fake_git$url("/pak-test.git"),
      "badcafe"
    ))
  )
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

test_that("git_download_repo", {
  skip_on_cran()
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  git_download_repo(
    fake_git$url("/pak-test.git"),
    ref = "v1",
    output = file.path(tmp, "v1")
  )
  expect_snapshot(dir(tmp, recursive = TRUE))
})

test_that("unpack_packfile_repo", {
  # reordering objects in a PACK file does not matter
  ref <- "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
  pack <- git_fetch(fake_git$url("/pak-test.git"), ref, blobs = TRUE)

  withr::local_seed(13L)
  pack <- sample(pack)

  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  unpack_packfile_repo(pack, output, url = "url")
  expect_snapshot(
    sort(dir(output, recursive=TRUE))
  )
})
