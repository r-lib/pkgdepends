
test_that("parse_remote_git", {
  expect_snapshot(parse_remote_git("git::https://github.com/r-lib/cli"))
  expect_snapshot(parse_remote_git("git::https://github.com/r-lib/cli.git"))
  expect_snapshot(parse_remote_git("git::https://github.com/r-lib/cli@branch"))
  expect_snapshot(parse_remote_git("git::https://github.com/r-lib/cli.git@branch"))

  expect_snapshot(parse_remote_git("pkg=git::https://github.com/r-lib/cli"))
  expect_snapshot(parse_remote_git("pkg=git::https://github.com/r-lib/cli.git"))
  expect_snapshot(parse_remote_git("pkg=git::https://github.com/r-lib/cli@branch"))
  expect_snapshot(parse_remote_git("pkg=git::https://github.com/r-lib/cli.git@branch"))
})

test_that("resolve_remote_git", {
  skip_on_cran()
  setup_fake_apps()
  prop <- suppressMessages(new_pkg_installation_proposal(
    "git::https://github.com/r-lib/cli@v3.6.0",
    config = list(library = tempfile())
  ))
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()
  attr(res, "metadata")$resolution_start <- NULL
  attr(res, "metadata")$resolution_end <- NULL
  expect_snapshot(
    as.list(res),
    variant = paste0("pillar-", packageVersion("pillar"))
  )
})

test_that("download_remote_git", {
  skip_on_cran()
  setup_fake_apps()
  prop <- new_pkg_installation_proposal("git::https://github.com/r-lib/cli@v3.6.0")
  suppressMessages(prop$solve())
  suppressMessages(prop$download())
  expect_snapshot(
    dir(file.path(prop$get_downloads()$fulltarget_tree, "cli"))
  )
  expect_equal(
    desc::desc_get(
      "Version",
      file.path(prop$get_downloads()$fulltarget_tree, "cli")
    ),
    c(Version = "3.6.0")
  )
  expect_equal(prop$get_downloads()$download_status, "Got")
})

test_that("satisfy_remote_git", {
  expect_equal(
    satisfy_remote_git(list(package = "foo"), list(package = "bar")),
    structure(FALSE, reason = "Package names differ")
  )

  expect_equal(
    satisfy_remote_git(
      list(package = "foo", params = list(c("reinstall"=""))),
      list(type = "installed", package = "foo")
    ),
    structure(FALSE, reason = "Re-install requested")
  )

  expect_equal(
    satisfy_remote_git(
      list(
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      ),
      list(
        type = "installed",
        package = "foo",
        extra = list(list(remotesha = "badbadcafe"))
      )
    ),
    structure(FALSE, reason = "Installed package sha mismatch")
  )

  expect_true(
    satisfy_remote_git(
      list(
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      ),
      list(
        type = "installed",
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      )
    )
  )

  expect_true(
    satisfy_remote_git(
      list(package = "foo"),
      list(type = "local", package = "foo")
    )
  )

  expect_equal(
    satisfy_remote_git(
      list(
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      ),
      list(
        type = "github",
        package = "foo",
        extra = list(list(remotesha = "badbadcafe"))
      )
    ),
    structure(FALSE, reason = "Candidate package sha mismatch")
  )

  expect_true(
    satisfy_remote_git(
      list(
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      ),
      list(
        type = "github",
        package = "foo",
        extra = list(list(remotesha = "badcafe"))
      )
    )
  )
})

test_that("installedok_remote_git", {
  expect_true(
    installedok_remote_git(
      list(package = "foo", version = "1.0.0", remotesha = "badcafe"),
      list(
        package = "foo",
        version = "1.0.0",
        metadata = list(c("RemoteSha" = "badcafe"))
      )
    )
  )

  expect_false(
    installedok_remote_git(
      list(package = "foo", version = "1.0.0", remotesha = "badcafe"),
      list(
        package = "foo",
        version = "1.0.1",
        metadata = list(c("RemoteSha" = "badbadcafe"))
      )
    )
  )

  expect_false(
    installedok_remote_git(
      list(package = "foo", version = "1.0.0", remotesha = "badcafe"),
      list(
        package = "foo",
        version = "1.0.0",
        metadata = list(c("RemoteSha" = "badbadcafe"))
      )
    )
  )
})

test_that("git_auth_url", {
  mockery::stub(git_auth_url, "gitcreds_get", function(...) stop("oops"))
  expect_equal(
    git_auth_url(list(url = "https://github.com/r-lib/cli.git")),
    "https://github.com/r-lib/cli.git"
  )

  mockery::stub(
    git_auth_url,
    "gitcreds_get",
    list(username = "user", password = "secret")
  )
  expect_equal(
    git_auth_url(list(
      protocol = "https",
      url = "https://github.com/r-lib/cli.git"
    )),
    "https://user:secret@github.com/r-lib/cli.git"
  )
})

test_that("type_git_get_data", {
  # TODO
  expect_true(TRUE)
})

test_that("type_git_make_resolution", {
  # TODO
  expect_true(TRUE)
})
