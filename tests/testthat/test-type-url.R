test_that("url remote basics", {
  setup_fake_apps()

  url <- paste0(
    fake_cran$url(),
    "/src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz"
  )
  r <- suppressMessages(new_pkg_installation_proposal(
    paste0("url::", url),
    config = list(library = tempfile())
  ))

  suppressMessages(r$resolve())
  suppressMessages(r$solve())
  suppressMessages(r$download())

  expect_snapshot(
    r$get_solution(),
    transform = transform_local_port
  )

  dl <- r$get_downloads()
  expect_null(dl$download_error[[1]])
  expect_true(file.exists(dl$fulltarget))
})

test_that("url to an tree (not a source package), from a zip file", {
  setup_fake_gh_app()
  url <- paste0(
    fake_gh$url(),
    "repos/r-lib/pak/zipball/",
    "111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a"
  )
  r <- suppressMessages(new_pkg_installation_proposal(
    paste0("url::", url),
    config = list(library = tempfile())
  ))

  suppressMessages(r$resolve())
  suppressMessages(r$solve())
  suppressMessages(r$download())

  expect_snapshot(
    r$get_solution(),
    transform = transform_local_port
  )

  dl <- r$get_downloads()
  expect_null(dl$download_error[[1]])
  expect_true(file.exists(dl$fulltarget_tree))
})

test_that("url remote and install plan", {
  # We test this separately, because it is slightly different.
  # We need to download the file both at resolution and also at
  # downloads.
  setup_fake_apps()

  url <- paste0(
    fake_cran$url(),
    "/src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz"
  )
  r <- suppressMessages(new_pkg_installation_proposal(
    paste0("url::", url),
    config = list(library = tempfile())
  ))
  lockfile <- tempfile()
  on.exit(unlink(lockfile), add = TRUE)
  r$resolve()
  r$solve()
  r$create_lockfile(lockfile)

  tmpdir <- type_url_tempdir(r$get_resolution()$remote[[1]], r$get_config())
  rimraf(c(tmpdir$archive, tmpdir$extract, tmpdir$ok))

  plan <- new_pkg_installation_plan(lockfile)
  suppressMessages(plan$download())
  expect_null(plan$get_downloads()$download_error[[1]])
  file.exists(plan$get_downloads()$fulltarget)
})

test_that("satisfy", {
  # different package name is bad (although not really possible)
  expect_snapshot(satisfy_remote_url(
    list(package = "package"),
    list(package = "other")
  ))

  # installed, reinstall requested?
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      params = list(c(reinstall = ""))
    ),
    list(
      type = "installed",
      package = "package"
    )
  ))

  # installed, no etag
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      metadata = list(list(RemoteEtag = "myetag"))
    ),
    list(
      type = "installed",
      package = "package"
    )
  ))

  # installed, etag mismatch
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      metadata = list(list(RemoteEtag = "myetag"))
    ),
    list(
      type = "installed",
      package = "package",
      extra = list(list(remoteetag = "otheretag"))
    )
  ))

  # installed, etag match
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      metadata = list(list(RemoteEtag = "myetag"))
    ),
    list(
      type = "installed",
      package = "package",
      extra = list(list(remoteetag = "myetag"))
    )
  ))

  # url, url mismatch
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      ref = "https://myurl"
    ),
    list(
      type = "url",
      package = "package",
      ref = "https://anotherurl"
    )
  ))

  # url, url match
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      ref = "https://myurl"
    ),
    list(
      type = "url",
      package = "package",
      ref = "https://myurl"
    )
  ))

  # other type of remote
  expect_snapshot(satisfy_remote_url(
    list(
      package = "package",
      ref = "https://myurl"
    ),
    list(
      type = "local",
      package = "package"
    )
  ))
})

test_that("nocache", {
  setup_fake_apps()

  url <- paste0(
    fake_cran$url(),
    "/src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz"
  )
  r <- suppressMessages(new_pkg_installation_proposal(
    paste0("url::", url, "?nocache"),
    config = list(library = tempfile())
  ))

  suppressMessages(r$resolve())
  suppressMessages(r$solve())
  suppressMessages(r$download())

  expect_snapshot(
    r$get_solution(),
    transform = transform_local_port
  )

  dl <- r$get_downloads()
  expect_null(dl$download_error[[1]])
  expect_true(file.exists(dl$fulltarget))
})

test_that("bad archive file (multiple directories)", {
  mkdirp(tmp <- tempfile())
  withr::local_dir(tmp)
  mkdirp("pkg1")
  mkdirp("pkg2")
  expect_snapshot(
    error = TRUE,
    get_pkg_dir_from_archive_dir(tmp),
    transform = transform_tempdir
  )
})

test_that("installedok", {
  expect_true(installedok_remote_url(
    list(
      package = "package",
      version = "1.0.0",
      remotetype = "url",
      remoteetag = "myetag"
    ),
    list(
      package = "package",
      version = "1.0.0",
      metadata = list(list("RemoteEtag" = "myetag"))
    )
  ))

  expect_false(installedok_remote_url(
    list(
      package = "package",
      version = "1.0.0",
      remotetype = "url",
      remoteetag = "myetag"
    ),
    list(
      package = "package",
      version = "1.0.0",
      metadata = list(list("RemoteEtag" = "otheretag"))
    )
  ))
})
