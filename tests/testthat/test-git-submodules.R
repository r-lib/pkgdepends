withr::local_envvar(GITHUB_PAT = "FAIL")

test_that("parse_submodules", {
  sm <- test_path("fixtures/submodules.ini")
  expect_snapshot({
    parse_submodules(sm)
    parse_submodules(read_char(sm))
  })

  file.create(tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  expect_snapshot({
    parse_submodules(tmp)
  })

  # invalid file
  sm2 <- test_path("fixtures/submodules2.ini")
  sm3 <- test_path("fixtures/submodules3.ini")
  expect_snapshot({
    parse_submodules(sm2)
    parse_submodules(sm3)
  })
})

test_that("git_download_repo with submodules", {
  skip_on_cran()
  if (Sys.which("git") == "") skip("Needs git")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  output <- file.path(tmp, "v1")
  git_download_repo(
    fake_git$url("/pak-test.git"),
    output = output
  )

  # tag
  writeLines(
    c(
      "[submodule \"submod\"]",
      "\tpath = submod",
      paste0("\turl = ", fake_git$url("/submod")),
      "\tbranch = v2"
    ),
    file.path(output, ".gitmodules")
  )

  update_git_submodules(output)
  expect_snapshot(dir(tmp, recursive = TRUE))
  expect_snapshot(readLines(file.path(output, "submod", "README")))

  # HEAD
  unlink(file.path(output, "submod"), recursive = TRUE)
  writeLines(
    c(
      "[submodule \"submod\"]",
      "\tpath = submod",
      paste0("\turl = ", fake_git$url("/submod"))
    ),
    file.path(output, ".gitmodules")
  )

  update_git_submodules(output)
  # it will skip existing ones
  update_git_submodules(output)
  expect_snapshot(dir(tmp, recursive = TRUE))
  expect_snapshot(readLines(file.path(output, "submod", "README")))
})

test_that("git_download_repo R package with submodules", {
  skip_on_cran()
  if (Sys.which("git") == "") skip("Needs git")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  output <- file.path(tmp, "v1")
  git_download_repo(
    fake_git$url("/pak-test.git"),
    output = output
  )

  # tag
  writeLines(
    c(
      "[submodule \"submod\"]",
      "\tpath = submod",
      paste0("\turl = ", fake_git$url("/submod")),
      "\tbranch = v2"
    ),
    file.path(output, ".gitmodules")
  )

  update_git_submodules_r(output, ".")
  expect_snapshot(dir(tmp, recursive = TRUE))
  expect_snapshot(readLines(file.path(output, "submod", "README")))

  # HEAD
  unlink(file.path(output, "submod"), recursive = TRUE)
  writeLines(
    c(
      "[submodule \"submod\"]",
      "\tpath = submod",
      paste0("\turl = ", fake_git$url("/submod"))
    ),
    file.path(output, ".gitmodules")
  )

  update_git_submodules_r(output, ".")
  # it will skip existing ones
  update_git_submodules_r(output, ".")
  expect_snapshot(dir(tmp, recursive = TRUE))
  expect_snapshot(readLines(file.path(output, "submod", "README")))

  # no submodule file
  unlink(file.path(output, "submod"), recursive = TRUE)
  unlink(file.path(output, ".gitmodules"))
  update_git_submodules_r(output, ".")
  expect_snapshot(dir(tmp, recursive = TRUE))

  # Empty submodule file
  unlink(file.path(output, "submod"), recursive = TRUE)
  writeLines(character(), file.path(output, ".gitmodules"))
  update_git_submodules_r(output, ".")
  expect_snapshot(dir(tmp, recursive = TRUE))
})

test_that("git_download_repo R package with ignored submodule", {
  skip_on_cran()
  if (Sys.which("git") == "") skip("Needs git")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  output <- file.path(tmp, "v1")
  git_download_repo(
    fake_git$url("/pak-test.git"),
    ref = "build-ignore",
    output = output
  )

  update_git_submodules_r(output, ".")
  expect_snapshot(dir(tmp, recursive = TRUE, all.files = TRUE, no.. = TRUE))
})

test_that("directories", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)

  mkdirp("a/b/c/d")
  expect_snapshot({
    directories("a")
    directories("a/b/c/d")
  })
})
