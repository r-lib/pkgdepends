withr::local_envvar(GITHUB_PAT="FAIL")

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
  writeLines(c(
    "[submodule \"submod\"]",
    "\tpath = submod",
    paste0("\turl = ", fake_git$url("/submod")),
    "\tbranch = v2"
  ), file.path(output, ".gitmodules"))

  update_git_submodules(output)
  expect_snapshot(dir(tmp, recursive = TRUE))
  expect_snapshot(readLines(file.path(output, "submod", "README")))

  # HEAD
  unlink(file.path(output, "submod"), recursive = TRUE)
  writeLines(c(
    "[submodule \"submod\"]",
    "\tpath = submod",
    paste0("\turl = ", fake_git$url("/submod"))
  ), file.path(output, ".gitmodules"))

  update_git_submodules(output)
  expect_snapshot(dir(tmp, recursive = TRUE))
  expect_snapshot(readLines(file.path(output, "submod", "README")))
})
