
test_that("parse_pkg_refs, github", {

  cases <- list(
    list("user/repo"),
    list("github::user/repo"),
    list("pkg=user/repo", package = "pkg"),
    list("pkg=github::user/repo", package = "pkg"),
    list("user/repo/subdir", subdir = "subdir"),
    list("user/repo@badcafe", commitish = "badcafe"),
    list("user/repo#123", pull = "123"),
    list("user/repo@*release", release = "*release"),
    list("github::user/repo/subdir", subdir = "subdir"),
    list("github::user/repo@badcafe", commitish = "badcafe"),
    list("github::user/repo#123", pull = "123"),
    list("github::user/repo@*release", release = "*release"),
    list("pkg=user/repo/subdir", package = "pkg", subdir = "subdir"),
    list("pkg=user/repo@badcafe", package = "pkg", commitish = "badcafe"),
    list("pkg=user/repo#123", package = "pkg", pull = "123"),
    list("pkg=user/repo@*release", package = "pkg", release = "*release"),

    # github url cases
    list("git@github.com:user/repo.git"),
    list("git@github.ubc.ca:user/repo.git"),
    list("https://github.com/user/repo"),
    list("https://github.ubc.ca/user/repo"),
    list("https://github.com/user/repo/tree/i-am-a-branch", commitish = "i-am-a-branch"),
    list("https://github.com/user/repo/commit/1234567", commitish = "1234567"),
    list("https://github.com/user/repo/pull/108", pull = "108"),
    list("https://github.com/user/repo/releases/tag/1.0.0", commitish = "1.0.0"),
    list("https://github.com/user/repo/releases/latest", release = "latest"),
    list("https://github.com/user/repo/releases/latest", release = "latest"),
    list("https://github.com/foo/bar", username = "foo", repo = "bar"),
    list("git@github.com:foo/bar.git", username = "foo", repo = "bar"),

    # Username and repo can have hyphens in them
    list("git@github.com:foo-bar/baz-qux.git", username = "foo-bar", repo = "baz-qux")
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_pkg_refs(case[[1]])[[1]],
      utils::modifyList(
        list(package = case$repo %||% "repo", username = "user",
             repo = "repo", subdir = "", commitish = "", pull = "",
             release = "", ref = case[[1]], type = "github",
             params = character()),
        case[-1]
      )
    )
    expect_s3_class(p, c("remote_ref_github", "remote_ref"))
  }
})

test_that("github url regexes", {
  cases <- list(
    list("https://github.com/u/repo.git", c(username = "u", repo = "repo")),
    list("https://github.com/u/re.po", c(username = "u", repo = "re.po")),
    list("https://github.com/u/re.po.git", c(username = "u", repo = "re.po"))
  )
  for (c in cases) {
    m <- re_match(c[[1]], github_url_rx())
    for (n in names(c[[2]])) expect_equal(c[[2]][[n]], m[[n]])
  }
})

test_that("resolve_remote", {
  setup_fake_apps()
  setup_fake_gh_app()

  refs <- c(
    "gaborcsardi/secret",
    "gaborcsardi/secret@x",
    "r-lib/crayon",
    "github::r-lib/crayon",
    "crayon=r-lib/crayon",
    "crayon=github::r-lib/crayon",
    "wesm/feather/R",
    "r-lib/crayon@b5221ab0246050",
    "r-lib/crayon#79",
    # TODO: fix "r-lib/testthat@*release",
    "r-lib/pkgconfig#7"
  )

  # set library to avoid installed packages
  suppressMessages(plan <- new_pkg_installation_proposal(
    refs,
    config = list(library = tempfile())
  ))
  suppressMessages(plan$resolve())

  res <- plan$get_resolution()
  res <- res[order(res$ref), ]

  res[["sources"]] <- sub("^http://127.0.0.1:[0-9]+/", "", unlist(res$sources))
  expect_snapshot(res[, c("ref", "sources")])

  expect_snapshot(
    res$metadata,
    transform = transform_local_port
  )
})

test_that("failed resolution", {
  setup_fake_gh_app()

  nonrepo <- paste0(basename(tempfile()), "/", basename(tempfile()))
  r <- pkg_plan$new(nonrepo)
  suppressMessages(r$resolve())
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))

  ## Existing repo, no R package there

  r <- pkg_plan$new("github::r-lib/crayon/R")
  suppressMessages(r$resolve())
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))
})

test_that("download_remote", {
  setup_fake_gh_app()

  old_cache <- Sys.getenv("R_PKG_CACHE_DIR")
  on.exit(Sys.setenv(R_PKG_CACHE_DIR = old_cache))
  Sys.setenv(R_PKG_CACHE_DIR = cache <- tempfile())
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sha <- "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2"
  ref <- paste0("github::r-lib/crayon@", sha)
  r <- pkg_plan$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))

  ## -----------------------------------------------------
  ## We get the tree zip first
  expect_error(suppressMessages(r$resolve()), NA)
  expect_error(suppressMessages(r$download_resolution()), NA)
  dl <- r$get_resolution_download()

  expect_s3_class(dl, "pkgplan_downloads")
  expect_true(dl$ref == ref)
  expect_true(dl$type == "github")
  expect_true(dl$direct)
  expect_true(dl$status == "OK")
  expect_true(dl$package == "crayon")
  expect_true(dl$download_status == "Got")

  ## We indeed downloaded a tree
  expect_false(file.exists(dl$fulltarget))
  expect_true(file.exists(dl$fulltarget_tree))

  ## It was added to the cache
  expect_equal(pkgcache::pkg_cache_list()$sha256, sha)

  ## -----------------------------------------------------
  ## Now the tree is in the cache, so it should come from there
  r <- pkg_plan$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))

  expect_error(suppressMessages(r$resolve()), NA)
  expect_error(suppressMessages(r$download_resolution()), NA)
  dl <- r$get_resolution_download()

  expect_s3_class(dl, "pkgplan_downloads")
  expect_true(dl$ref == ref)
  expect_true(dl$type == "github")
  expect_true(dl$direct)
  expect_true(dl$status == "OK")
  expect_true(dl$package == "crayon")
  expect_true(dl$download_status == "Had")

  expect_false(file.exists(dl$fulltarget))
  expect_true(file.exists(dl$fulltarget_tree))

  ## Still in the cache
  expect_equal(pkgcache::pkg_cache_list()$sha256, sha)

  ## -----------------------------------------------------
  ## Put a (fake) built version in the cache, to similate a cache hit
  file.copy(dl$fulltarget_tree, dl$fulltarget)
  pkgcache::pkg_cache_add_file(
    NULL, dl$fulltarget, dl$target, package = "crayon", sha = sha,
    built = TRUE, vignettes = TRUE)
  unlink(c(dl$fulltarget, dl$fulltarget_tree))

  r <- pkg_plan$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))

  expect_error(suppressMessages(r$resolve()), NA)
  expect_error(suppressMessages(r$download_resolution()), NA)
  dl <- r$get_resolution_download()

  expect_s3_class(dl, "pkgplan_downloads")
  expect_true(dl$ref == ref)
  expect_true(dl$type == "github")
  expect_true(dl$direct)
  expect_true(dl$status == "OK")
  expect_true(dl$package == "crayon")
  expect_true(dl$download_status == "Had")

  expect_true(file.exists(dl$fulltarget))
  expect_false(file.exists(dl$fulltarget_tree))
})

test_that("satisfies_remote", {

  res <- make_fake_resolution(`github::r-lib/crayon` = list(
    extra = list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0"))

  ## Different package name
  bad1 <- make_fake_resolution(`github::r-lib/crayon` = list(
    package = "crayon2"))
  expect_false(ans <- satisfy_remote_github(res, bad1))
  expect_match(attr(ans, "reason"), "names differ")

  ## Installed ref without sha
  fake_desc <- desc::desc("!new")
  bad2 <- make_fake_resolution(`installed::foobar` = list(
    extra = list(list(description = fake_desc)),
    package = "crayon"))
  expect_false(ans <- satisfy_remote_github(res, bad2))
  expect_match(attr(ans, "reason"), "Installed package sha mismatch")

  ## Installed ref with different sha
  fake_desc <- desc::desc("!new")
  fake_desc$set("RemoteSha" = "notsobad")
  bad3 <- make_fake_resolution(`installed::foobar` = list(
    extra = list(list(description = fake_desc)),
    package = "crayon"))
  expect_false(ans <- satisfy_remote_github(res, bad3))
  expect_match(attr(ans, "reason"), "Installed package sha mismatch")

  ## Other package, different sha
  bad4 <- make_fake_resolution(`bioc::bar` = list(
    package = "crayon",
    extra = list(list(remotesha = "notsobad"))))
  expect_false(ans <- satisfy_remote_github(res, bad4))
  expect_match(attr(ans, "reason"), "Candidate package sha mismatch")

  ## Corrent sha, GitHub
  ok1 <- make_fake_resolution(`installed::foo` = list(
    extra =  list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0"))
  expect_true(satisfy_remote_github(res, ok1))

  ## Corrent sha, another type
  ok2 <- make_fake_resolution(`local::bar` = list(
    package = "crayon",
    extra = list(list(remotesha = "badcafe"))))
  expect_true(ans <- satisfy_remote_github(res, ok2))

  ## Local packages are OK, even w/o sha
  ## https://github.com/r-lib/pkgdepends/issues/229
  ok3 <- make_fake_resolution(`local::bar` = list(
    package = "crayon"
  ))
  expect_true(ans <- satisfy_remote_github(res, ok3))
})

test_that("satisfies_remote 2", {
  ## installed is not OK, if reinstall is requested
  res <- make_fake_resolution(`github::r-lib/crayon?reinstall` = list(
    extra = list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0",
    params = c(reinstall = "")
  ))
  bad1 <- make_fake_resolution(`installed::foo` = list(
    extra =  list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0"))
  expect_false(ans <- satisfy_remote_github(res, bad1))
})

test_that("satisfies_remote 3", {
  # other refs are OK, as long as the sha matches
  ## installed is not OK, if reinstall is requested
  res <- make_fake_resolution(`github::r-lib/crayon` = list(
    extra = list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0",
    params = c(reinstall = "")
  ))

  ok1 <- make_fake_resolution(`url::foo` = list(
    extra =  list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0"))
  expect_true(ans <- satisfy_remote_github(res, ok1))

  bad1 <- make_fake_resolution(`url::foo` = list(
    extra =  list(list(remotesha = "baddddd")),
    package = "crayon",
    version = "1.0.0"))
  expect_false(ans <- satisfy_remote_github(res, bad1))
})

test_that("installedok", {
  expect_true(installedok_remote_github(
    list(
      package = "foo",
      version = "1.2.3",
      remotesha = "badcafe"
    ),
    list(
      package = "foo",
      version = "1.2.3",
      metadata = list(list(RemoteSha = "badcafe"))
    )
  ))

  expect_false(installedok_remote_github(
    list(
      package = "foo",
      version = "1.2.3",
      remotesha = "badcafe"
    ),
    list(
      package = "foo",
      version = "1.2.3"
    )
  ))
})
