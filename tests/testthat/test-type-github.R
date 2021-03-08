
test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  refs <- c(
    "gaborcsardi/secret",
    "gaborcsardi/secret@x",
    "r-lib/crayon",
    "github::r-lib/crayon",
    "crayon=r-lib/crayon",
    "crayon=github::r-lib/crayon",
    "wesm/feather/R",
    "r-lib/crayon@b5221ab0246050",
    "r-lib/crayon#61",
    "r-lib/testthat@*release",
    "r-lib/pkgconfig#7"
  )

  r <- pkg_plan$new(
    refs, config = list(dependencies = FALSE, cache_dir = tmp))
  expect_error(suppressMessages(r$resolve()), NA)
  res <- r$get_resolution()

  expect_s3_class(res, "pkg_resolution_result")
  expect_equal(sort(res$ref), sort(refs))
  expect_true(all(res$type == "github"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))

  ord <- match(refs, vcapply(res$metadata, "[[", "RemotePkgRef"))

  expect_true(all(vcapply(res$metadata, "[[", "RemoteType") == "github"))
  expect_equal(vcapply(res$metadata, "[[", "RemotePkgRef")[ord], refs)
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteSha")[ord][8],
    "b5221ab0246050dc687dc8b9964d5c44c947b265")
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteUsername")[ord],
    c("gaborcsardi", "gaborcsardi", rep("r-lib", 4), "wesm", rep("r-lib", 4)))
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteRepo")[ord],
    c("secret", "secret", rep("crayon", 4), "feather", "crayon",
      "crayon", "testthat", "pkgconfig"))
  expect_equal(
    vcapply(res$metadata, "[", "RemoteSubdir")[ord],
    c(NA, NA, NA, NA, NA, NA, "R", NA, NA, NA, NA))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteHost") == "api.github.com"))
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteSha")[ord][11],
    "6c61f82a5c793c250a28f02a7ef14ae52eb83336")

  expect_equal(vcapply(res$metadata, "[[", "GithubRepo"),
               vcapply(res$metadata, "[[", "RemoteRepo"))
  expect_equal(vcapply(res$metadata, "[[", "GithubUsername"),
               vcapply(res$metadata, "[[", "RemoteUsername"))
  expect_equal(vcapply(res$metadata, "[", "GithubRef"),
               vcapply(res$metadata, "[", "RemoteRef"))
  expect_equal(vcapply(res$metadata, "[[", "GithubSHA1"),
               vcapply(res$metadata, "[[", "RemoteSha"))
  expect_equal(vcapply(res$metadata, "[", "GithubSubdir"),
               vcapply(res$metadata, "[", "RemoteSubdir"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  nonrepo <- paste0(basename(tempfile()), "/", basename(tempfile()))
  r <- pkg_plan$new(
    nonrepo, config = list(dependencies = FALSE, cache_dir = tmp))
  expect_error(r$resolve(), NA)
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))

  ## Existing repo, no R package there

  r <- pkg_plan$new(
    "github::r-lib/crayon/R", config = list(cache_dir = tmp))
  expect_error(r$resolve(), NA)
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))
})

test_that("download_remote", {

  skip_if_offline()
  skip_on_cran()

  old_cache <- Sys.getenv("R_PKG_CACHE_DIR")
  on.exit(Sys.setenv(R_PKG_CACHE_DIR = old_cache))
  Sys.setenv(R_PKG_CACHE_DIR = cache <- tempfile())
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  sha <- "b5221ab0246050dc687dc8b9964d5c44c947b265"
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
