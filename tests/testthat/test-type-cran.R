
test_that("resolve_remote", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal("pkg1")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve_remote, multiple", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal(c("cran::pkg3", "pkg1"))
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("dependencies", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal("pkg3")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("failed resolution", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal("cran::xxyyzzqwertyqwerty")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("failed resolution, multiple", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal(c("cran::pkg1", "cran::xxyyzzqwertyqwerty"))
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve current version", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal("cran::pkg1@current")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve an old version", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal("pkg1@0.9.0")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = function(x) transform_no_srcref(fix_port(x)))

  prop <- new_pkg_installation_proposal("pkg1@1.0.0")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = function(x) transform_no_srcref(fix_port(x)))
})

test_that("resolve a version range", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  prop <- new_pkg_installation_proposal("pkg1@>=0.9.0")
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
