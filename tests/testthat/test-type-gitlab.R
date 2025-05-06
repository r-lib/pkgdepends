test_that("resolve", {
  skip_on_cran()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  p <- suppressMessages(new_pkg_installation_proposal(
    "gitlab::gaborcsardi/cli",
    config = list(library = tmp)
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot({
    res$error
    res$package
    res$version
    res$metadata[[1]]
  })

  # group + branch
  p <- suppressMessages(new_pkg_installation_proposal(
    "gitlab::r-hub/filelock@cran-1-0-2",
    config = list(library = tmp)
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot({
    res$error
    res$package
    res$version
    res$metadata[[1]]
  })

  # tag
  p <- suppressMessages(new_pkg_installation_proposal(
    "gitlab::r-hub/filelock@v1.0.2",
    config = list(library = tmp)
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot({
    res$error
    res$package
    res$version
    res$metadata[[1]]
  })

  # subdirectory
  p <- suppressMessages(new_pkg_installation_proposal(
    "gitlab::gaborcsardi/feather/-/R",
    config = list(library = tmp, dependencies = FALSE)
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot({
    res$error
    res$package
    res$version
    res$metadata[[1]]
  })
})

test_that("download", {
  skip_on_cran()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # subdirectory
  p <- suppressMessages(new_pkg_installation_proposal(
    "gitlab::gaborcsardi/feather/-/R",
    config = list(library = tmp, dependencies = FALSE)
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  suppressMessages(p$solve())
  suppressMessages(p$download())
  dl <- p$get_downloads()
  expect_true(dir.exists(dl$fulltarget_tree))
  expect_snapshot(
    dir(file.path(dl$fulltarget_tree, "feather", "R"))
  )
})

test_that("satisfy", {
  expect_true(
    satisfy_remote_gitlab(
      list(
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      ),
      list(
        type = "gitlab",
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      )
    )
  )
})

test_that("installedok", {
  expect_true(
    installedok_remote_gitlab(
      list(package = "foo", version = "1.0.0", remotesha = "badcafe"),
      list(
        package = "foo",
        version = "1.0.0",
        metadata = list(c("RemoteSha" = "badcafe"))
      )
    )
  )
})
