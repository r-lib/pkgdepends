
context("local ref type")

test_that("parse_remote", {
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  pr <- parse_remotes(ref)[[1]]
  expect_true(is.list(pr))
  expect_equal(pr$path, path)
  expect_equal(pr$ref, ref)
  expect_equal(pr$type, "local")
})

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(c(pkg.progress.bar = FALSE), {
    r$resolve()
  })
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(res$data$ref == ref)
  expect_true(res$data$type == "local")
  expect_true(res$data$direct)
  expect_true(res$data$status == "OK")
  expect_true(res$data$package == "foobar")
  expect_true(res$data$version == "1.0.0")

  ## Relative path?
  fix_dir <- fixture_dir()
  ref2 <- paste0("local::", "foobar_1.0.0.tar.gz")
  r <- remotes$new(
    ref2, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_dir(fix_dir,
    withr::with_options(c(pkg.progress.bar = FALSE), {
      r$resolve()
    })
  )
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(res$data$ref == ref2)
  expect_true(res$data$type == "local")
  expect_true(res$data$direct)
  expect_true(res$data$status == "OK")
  expect_true(res$data$package == "foobar")
  expect_true(res$data$version == "1.0.0")
  expect_true(res$data$sources[[1]] == path)
})

test_that("resolution error", {

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- get_fixture("foobar_10.0.0.tar.gz")
  ref <- paste0("local::", path)
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(c(pkg.progress.bar = FALSE), {
    r$resolve()
  })
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(res$data$ref == ref)
  expect_true(res$data$type == "local")
  expect_true(res$data$direct)
  expect_true(res$data$status == "FAILED")
  expect_true(is.na(res$data$package))
  expect_true(is.na(res$data$version))
  expect_true(res$data$sources[[1]] == path)
})

test_that("download_remote", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(c(pkg.progress.bar = FALSE), {
    expect_error(r$resolve(), NA)
    expect_error(r$download_resolution(), NA)
  })
  dl <- r$get_resolution_download()

  expect_true(file.exists(dl$data$fulltarget))
  expect_s3_class(dl, "remotes_downloads")
  expect_true(all(dl$data$ref == ref))
  expect_true(all(dl$data$type == "local"))
  expect_true(all(dl$data$direct))
  expect_true(all(dl$data$status == "OK"))
  expect_true(all(dl$data$package == "foobar"))
  expect_true(all(dl$download_status == "Had"))

  ## Relative path?
  fix_dir <- fixture_dir()
  ref2 <- paste0("local::", "foobar_1.0.0.tar.gz")
  r <- remotes$new(
    ref2, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_dir(fix_dir,
    withr::with_options(c(pkg.progress.bar = FALSE), {
      expect_error(r$resolve(), NA)
      expect_error(r$download_resolution(), NA)
    })
  )
  dl <- r$get_resolution_download()

  expect_true(file.exists(dl$data$fulltarget))
  expect_s3_class(dl, "remotes_downloads")
  expect_true(all(dl$data$ref == ref2))
  expect_true(all(dl$data$type == "local"))
  expect_true(all(dl$data$direct))
  expect_true(all(dl$data$status == "OK"))
  expect_true(all(dl$data$package == "foobar"))
  expect_true(all(dl$download_status == "Had"))
})

test_that("download_remote error", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

  path <- get_fixture("foobar_1.0.0.tar.gz")
  file.copy(path, tmp2)
  ref <- paste0("local::", path2 <- file.path(tmp2, basename(path)))
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(c(pkg.progress.bar = FALSE), {
    expect_error(r$resolve(), NA)
    unlink(path2)
    expect_error(r$download_resolution(), NA)
  })
  dl <- r$get_resolution_download()

  expect_false(file.exists(dl$data$fulltarget))
  expect_s3_class(dl, "remotes_downloads")
  expect_true(all(dl$data$ref == ref))
  expect_true(all(dl$data$type == "local"))
  expect_true(all(dl$data$direct))
  expect_true(all(dl$data$status == "OK"))
  expect_true(all(dl$data$package == "foobar"))
  expect_true(all(dl$download_status == "Failed"))
})

test_that("satisfies_remote", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(c(pkg.progress.bar = FALSE), {
    r$resolve()
  })
  res <- r$get_resolution()

  ## The rest of the arguments are not even used...
  expect_false(satisfies_remote(res$data$resolution[[1]]))
})
