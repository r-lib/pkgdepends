test_that("parse_remote", {
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  pr <- parse_pkg_ref(ref)
  expect_true(is.list(pr))
  expect_equal(pr$path, path)
  expect_equal(pr$ref, ref)
  expect_equal(pr$type, "local")
})

test_that("resolve_remote", {
  setup_fake_apps()

  conf <- current_config()
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  res <- synchronise(resolve_remote_local(
    parse_pkg_ref(ref),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  ))

  res$metadata <- as.list(res$metadata)
  # seems really hard to remove the path
  res$sources[[1]] <- basename(res$sources[[1]])
  expect_snapshot(res)

  ## Relative path?
  fix_dir <- fixture_dir()
  ref2 <- paste0("local::", "foobar_1.0.0.tar.gz")
  withr::with_dir(
    fix_dir,
    res <- synchronise(resolve_remote_local(
      parse_pkg_ref(ref2),
      TRUE,
      conf,
      cache,
      dependencies = FALSE
    ))
  )

  res$metadata <- as.list(res$metadata)
  # seems really hard to remove the path
  res$sources[[1]] <- basename(res$sources[[1]])
  expect_snapshot(res)
})

test_that("resolution error", {
  conf <- current_config()
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  path <- get_fixture("foobar_10.0.0.tar.gz")
  ref <- paste0("local::", path)
  err <- tryCatch(
    synchronise(
      resolve_remote_local(
        parse_pkg_ref(ref),
        TRUE,
        conf,
        cache,
        dependencies = FALSE
      )
    ),
    error = function(x) x
  )

  expect_s3_class(err, "error")
})

test_that("download_remote", {
  setup_fake_apps()

  dir.create(tmp <- tempfile())
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  conf <- current_config()
  conf$platforms <- "macos"
  conf$cache_dir <- tmp
  conf$package_cache_dir <- tmp2
  cache <- list(
    package = pkgcache::package_cache$new(conf$package_cache_dir),
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)

  rem <- pkg_plan$new(ref)
  suppressMessages(rem$resolve())
  res <- rem$get_resolution()

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-tree")
  mkdirp(dirname(target))
  download <- function(res) {
    download_remote_local(res, target, tree, conf, cache, on_progress = NULL)
  }
  dl1 <- synchronise(download(res[1, ]))
  expect_equal(dl1, "Got")
  expect_true(file.exists(target))

  ## Relative path?
  fix_dir <- fixture_dir()
  ref2 <- paste0("local::", "foobar_1.0.0.tar.gz")

  withr::with_dir(fix_dir, {
    rem <- pkg_plan$new(ref2)
    rem$resolve()
    res <- rem$get_resolution()
  })

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-tree")
  mkdirp(dirname(target))
  dl1 <- download_remote_local(
    res,
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  )

  expect_equal(dl1, "Got")
  expect_true(file.exists(target))
})

test_that("download_remote directory", {
  setup_fake_apps()

  dir.create(tmp <- tempfile())
  dir.create(tmp2 <- tempfile())
  dir.create(tmp3 <- tempfile())
  on.exit(unlink(c(tmp, tmp2, tmp3), recursive = TRUE), add = TRUE)

  conf <- current_config()
  conf$platforms <- "source"
  conf$cache_dir <- tmp
  conf$package_cache_dir <- tmp2
  cache <- list(
    package = pkgcache::package_cache$new(conf$package_cache_dir),
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  path <- get_fixture("foobar_1.0.0.tar.gz")
  untar(path, exdir = tmp3)
  path <- file.path(tmp3, "foobar")
  ref <- paste0("local::", path)

  rem <- pkg_plan$new(ref)
  suppressMessages(rem$resolve())
  res <- rem$get_resolution()

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-tree")
  mkdirp(dirname(target))
  download <- function(res) {
    download_remote_local(res, target, tree, conf, cache, on_progress = NULL)
  }
  dl1 <- synchronise(download(res[1, ]))
  expect_equal(dl1, "Got")
  expect_true(file.exists(tree))
  expect_true(is.dir(tree))
})

test_that("download_remote error", {
  setup_fake_apps()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

  path <- get_fixture("foobar_1.0.0.tar.gz")
  file.copy(path, tmp2)
  ref <- paste0("local::", path2 <- file.path(tmp2, basename(path)), "?nocache")
  r <- pkg_plan$new(
    ref,
    config = list(dependencies = FALSE, cache_dir = tmp)
  )
  r$resolve()
  unlink(path2)
  expect_snapshot(invisible(r$download_resolution()))
  dl <- r$get_resolution_download()

  expect_false(file.exists(dl$fulltarget))
  expect_s3_class(dl, "pkgplan_downloads")
  expect_true(all(dl$ref == sub("?nocache", "", ref, fixed = TRUE)))
  expect_true(all(dl$type == "local"))
  expect_true(all(dl$direct))
  expect_true(all(dl$status == "OK"))
  expect_true(all(dl$package == "foobar"))
  expect_true(all(dl$download_status == "Failed"))
})

test_that("satisfy", {
  expect_snapshot({
    "different package name, FALSE"
    satisfy_remote_local(
      list(package = "foo"),
      list(package = "bar")
    )

    "different type is bad"
    satisfy_remote_local(
      list(
        package = "foo",
        remote = list(list(package = NA_character_)),
        path = "/tmp/foo"
      ),
      list(
        type = "notgood",
        package = "foo",
        path = "/tmp/foo"
      )
    )

    "any package name is good, good"
    satisfy_remote_local(
      list(
        package = "foo",
        remote = list(list(package = NA_character_, path = "/tmp/foo"))
      ),
      list(
        type = "local",
        package = "foo",
        remote = list(list(path = "/tmp/foo"))
      )
    )

    "requested a different package, bad"
    satisfy_remote_local(
      list(
        package = "foo",
        remote = list(list(package = "bar", path = "/tmp/foo"))
      ),
      list(
        type = "local",
        package = "foo",
        remote = list(list(package = "bar", path = "/tmp/foo"))
      )
    )

    "different paths are bad"
    satisfy_remote_local(
      list(
        package = "foo",
        remote = list(list(package = "foo", path = "/tmp/foo"))
      ),
      list(
        type = "local",
        package = "foo",
        remote = list(list(package = "foo", path = "/tmp/foo2"))
      )
    )
  })
})

test_that("installedok", {
  ## Always FALSE, independently of arguments
  expect_false(installedok_remote_local())
})
