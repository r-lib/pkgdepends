
withr::local_envvar(GITHUB_PAT="FAIL")

test_that("git_list_refs", {
  skip_on_cran()
  expect_snapshot(
    git_list_refs("https://github.com/gaborcsardi/pak-test.git")$refs
  )

  # filter
  expect_snapshot(
    git_list_refs_v2(
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
  skip_on_cran()
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
  skip_on_cran()

  # force v1
  pack <- git_fetch(
    "https://github.com/gaborcsardi/pak-test.git",
    structure("cefdc0eebcd7f757efb9a80652fd8aaf1a87508e", protocol = "1")
  )

  if (!l10n_info()[["UTF-8"]]) skip("UTF-8 snapshot")
  expect_snapshot(cat(pack[[1]]$object))
})

test_that("git_list_refs_v1", {
  skip_on_cran()
  expect_snapshot(
    git_list_refs_v1("https://github.com/gaborcsardi/pak-test.git")$refs
  )
})

test_that("git_list_refs_v1_process_1", {
  skip_on_cran()
  resp <- readRDS(test_path("fixtures", "git-response-v1.rds"))
  expect_snapshot(
    git_list_refs_v1_process_1(
      resp,
      "https://github.com/gaborcsardi/pak-test.git",
      "refs/tags/v1"
    )$refs
  )
})

test_that("async_git_resolve_ref", {
  skip_on_cran()

  # branches w/o refs/heads/ prefix
  expect_snapshot(
    sy(async_git_resolve_ref(
      "https://github.com/gaborcsardi/pak-test.git",
      "main"
    ))
  )

  # tags w/o refs/rags/ prefix
  expect_snapshot(
    sy(async_git_resolve_ref(
      "https://github.com/gaborcsardi/pak-test.git",
      "v1"
    ))
  )

  # sha prefix
  expect_snapshot(
    sy(async_git_resolve_ref(
      "https://github.com/gaborcsardi/pak-test.git",
      "3f3b0b4ee8a0ff"
    ))
  )

  # unknown sha prefix -> error
  expect_snapshot(
    error = TRUE,
    sy(async_git_resolve_ref(
      "https://github.com/gaborcsardi/pak-test.git",
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
      "https://github.com/gaborcsardi/pak-test.git",
      "badcafe"
    ))
  )
})

test_that("git_download_repo", {
  skip_on_cran()
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  git_download_repo(
    "https://github.com/gaborcsardi/pak-test.git",
    ref = "v1",
    output = file.path(tmp, "v1")
  )
  expect_snapshot(dir(tmp, recursive = TRUE))
})
