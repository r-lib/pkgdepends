
test_that("resolve_remote", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal("pkg1", config = config)
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve_remote, multiple", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal(
    c("cran::pkg3", "pkg1"),
    config = config
  )
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("dependencies", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal("pkg3", config = config)
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("failed resolution", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal(
    "cran::xxyyzzqwertyqwerty",
    config = config
  )
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("failed resolution, multiple", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal(
    c("cran::pkg1", "cran::xxyyzzqwertyqwerty"),
    config = config
  )
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve current version", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal(
    "cran::pkg1@current",
    config = config
  )
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve an old version", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal("pkg1@0.9.0", config = config)
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = function(x) {
    transform_etag(transform_hash(transform_no_srcref(fix_port(x))))
  })

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal("pkg1@1.0.0", config = config)
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = function(x) transform_no_srcref(fix_port(x)))
})

test_that("resolve a version range", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  config <- list(sysreqs_platform = "unknown")
  prop <- new_pkg_installation_proposal("pkg1@>=0.9.0", config = config)
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = function(x) transform_no_srcref(fix_port(x)))
})

test_that("download_remote", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  # this the cache for getOption(repos)
  pkgcache::pkg_cache_delete_files()

  conf <- list(library = lib <- tempfile())
  prop <- suppressMessages(
    new_pkg_installation_proposal("cran::pkg1", config = conf)
  )
  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  dl <- suppressMessages(prop$download())

  expect_true(file.exists(dl$fulltarget))
  expect_false(file.exists(dl$fulltarget_tree))
  expect_equal(dl$download_status, "Got")
  expect_equal(dl$file_size, file.size(dl$fulltarget))
  expect_equal(dl$download_error, list(NULL))

  cache <- pkgcache::pkg_cache_summary()
  expect_equal(cache$files, 1)
  expect_equal(cache$size, dl$file_size)

  # second time it will be cached
  prop <- suppressMessages(
    new_pkg_installation_proposal("cran::pkg1", config = conf)
  )
  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  dl2 <- suppressMessages(prop$download())

  expect_true(file.exists(dl2$fulltarget))
  expect_false(file.exists(dl2$fulltarget_tree))
  expect_equal(dl2$download_status, "Had")
  expect_equal(dl2$file_size, file.size(dl2$fulltarget))
  expect_equal(dl2$download_error, list(NULL))

  pkgcache::pkg_cache_delete_files()
})

test_that("versioned cran", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  dsc <- desc::desc("!new")
  dsc$set_dep("pkg3", "Imports")
  dsc$set_dep("pkg1", "Imports")
  dsc$set_remotes(c("pkg3@0.9.9", "pkg1@1.0.0"))
  dir.create(tmp <- tempfile())
  dsc_path <- file.path(tmp, "local", "DESCRIPTION")
  dir.create(dirname(dsc_path))
  dir.create(lib <- tempfile())
  on.exit(unlink(c(tmp, lib), recursive = TRUE), add = TRUE)
  dsc$write(dsc_path)
  prop <- pkgdepends::new_pkg_deps(
    paste0("local::", dirname(dsc_path)),
    config = list(library = lib)
  )
  suppressMessages(prop$solve())

  expect_snapshot(
    prop$draw(),
    transform = function(x) {
      sub("local::.*$", "local::<path>", transform_bytes(x))
    }
  )
})
