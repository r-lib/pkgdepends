
test_that("resolve_remote", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal("pkg1")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve_remote, multiple", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal(c("cran::pkg3", "pkg1"))
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("dependencies", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal("pkg3")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("failed resolution", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal("cran::xxyyzzqwertyqwerty")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("failed resolution, multiple", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal(c("cran::pkg1", "cran::xxyyzzqwertyqwerty"))
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve current version", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal("cran::pkg1@current")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve an old version", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal("pkg1@0.9.0")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)

  prop <- new_pkg_installation_proposal("pkg1@1.0.0")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("resolve a version range", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

  prop <- new_pkg_installation_proposal("pkg1@>=0.9.0")
  suppressMessages(prop$resolve())
  res <- prop$get_resolution()

  expect_snapshot({
    snapshot(res, extra = "all")
  }, transform = fix_port)
})

test_that("download_remote", {
  withr::local_options(
    repos = c(CRAN = cran$url()),
    pkg.cran_metadata_url = cran$url(),
    pkg.use_bioconductor = FALSE
  )

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
