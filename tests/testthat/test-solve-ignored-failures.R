
test_that("failure in non-needed package is ignored", {
  setup_fake_gh_app()
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()

  lib <- withr::local_tempdir()
  p <- new_pkg_installation_proposal(
    c("needspak", "r-lib/pak"),
    config = list(library = lib)
  )
  suppressWarnings(p$solve())
  expect_snapshot(p$draw(), transform = transform_bytes)
})
