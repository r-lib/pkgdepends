# The GitLab ref type only differs from the plain git type in how the ref is
# parsed (group path, host, etc.); resolution and download delegate to the git
# code. So we point the GitLab refs at the local git server (serving the
# `pak-test.git` fixture as `<group `repo`>/pak-test`) instead of gitlab.com,
# and the tests work offline. See `fake_gitlab` in helper-apps.R.

test_that("resolve", {
  skip_on_cran()
  setup_fake_apps()
  local_fake_git_no_creds(fake_gitlab$url())
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # default branch
  p <- suppressMessages(new_pkg_installation_proposal(
    paste0("gitlab::", fake_gitlab$url("/repo/pak-test")),
    config = list(library = tmp, sysreqs_platform = "unknown")
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot(
    {
      res$error
      res$package
      res$version
      res$metadata[[1]]
    },
    transform = transform_local_port
  )

  # group + branch
  p <- suppressMessages(new_pkg_installation_proposal(
    paste0("gitlab::", fake_gitlab$url("/repo/pak-test"), "@build-ignore"),
    config = list(library = tmp, sysreqs_platform = "unknown")
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot(
    {
      res$error
      res$package
      res$version
      res$metadata[[1]]
    },
    transform = transform_local_port
  )

  # tag
  p <- suppressMessages(new_pkg_installation_proposal(
    paste0(
      "gitlab::",
      fake_gitlab$url("/repo/pak-test/-/subdir/dotenv"),
      "@v1"
    ),
    config = list(library = tmp, sysreqs_platform = "unknown")
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot(
    {
      res$error
      res$package
      res$version
      res$metadata[[1]]
    },
    transform = transform_local_port
  )

  # subdirectory
  p <- suppressMessages(new_pkg_installation_proposal(
    paste0(
      "gitlab::",
      fake_gitlab$url("/repo/pak-test/-/subdir/dotenv"),
      "@subdir"
    ),
    config = list(
      library = tmp,
      dependencies = FALSE,
      sysreqs_platform = "unknown"
    )
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  expect_snapshot(
    {
      res$error
      res$package
      res$version
      res$metadata[[1]]
    },
    transform = transform_local_port
  )
})

test_that("download", {
  skip_on_cran()
  setup_fake_apps()
  local_fake_git_no_creds(fake_gitlab$url())
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # subdirectory
  p <- suppressMessages(new_pkg_installation_proposal(
    paste0(
      "gitlab::",
      fake_gitlab$url("/repo/pak-test/-/subdir/dotenv"),
      "@subdir"
    ),
    config = list(
      library = tmp,
      dependencies = FALSE,
      sysreqs_platform = "unknown"
    )
  ))
  suppressMessages(p$resolve())
  res <- p$get_resolution()
  suppressMessages(p$solve())
  suppressMessages(p$download())
  dl <- p$get_downloads()
  expect_true(dir.exists(dl$fulltarget_tree))
  expect_snapshot(
    dir(file.path(dl$fulltarget_tree, "dotenv", "subdir", "dotenv"))
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
