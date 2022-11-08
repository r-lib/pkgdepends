
test_that("new_pkg_download_proposal", {
  setup_fake_apps()

  dl <- new_pkg_download_proposal(
    "pkg3",
    config = list(library = tempfile())
  )
  expect_snapshot(dl)
  expect_snapshot(dl$get_refs())
  expect_snapshot(sort(dl$get_config()$list()))

  suppressMessages(dl$resolve())
  expect_snapshot(dl)
  expect_snapshot(
    dl$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
  )

})

test_that("async_resolve", {
  setup_fake_apps()

  dl <- new_pkg_download_proposal(
    "pkg3",
    config = list(library = tempfile())
  )
  expect_snapshot(
    synchronize(
      dl$async_resolve()$
      then(function() "done")
    )
  )
  expect_snapshot(
    dl$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
  )
})

test_that("download", {
  setup_fake_apps()

  tmp <- withr::local_tempdir()
  dl <- new_pkg_download_proposal(
    "pkg3",
    config = list(library = tempfile(), cache_dir = tmp)
  )

  suppressMessages(dl$resolve())
  suppressMessages(dl$download())

  expect_snapshot(dir(tmp, recursive = TRUE, pattern = "[.]tar[.]gz$"))
})

test_that("async_download", {
  setup_fake_apps()

  tmp <- withr::local_tempdir()
  dl <- new_pkg_download_proposal(
    "pkg3",
    config = list(library = tempfile(), cache_dir = tmp)
  )

  suppressMessages(dl$resolve())
  suppressMessages(synchronize(dl$async_download()))

  expect_snapshot(dir(tmp, recursive = TRUE, pattern = "[.]tar[.]gz$"))

  expect_snapshot(dl$get_downloads()$target)

  expect_silent(dl$stop_for_download_error())
})
