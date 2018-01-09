
context("metadata cache")

test_that("CRANMetadataCache", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  afun <- async::async(function() {
    d1 <- async::http_get(
      "https://cloud.r-project.org/src/contrib/PACKAGES.gz",
      file = file.path(tmp, "PACKAGES.gz"))$
      then(async::http_stop_for_status)
    d2 <- async::http_get(
      "https://cloud.r-project.org/src/contrib/Meta/archive.rds",
      file = file.path(tmp, "archive.rds"))$
      then(async::http_stop_for_status)
    when_all(d1, d2)
  })
  async::synchronise(afun())

  cache <- CRANMetadataCache$new()
  expect_error(pkgs <- cache$get(file.path(tmp, "PACKAGES.gz")), NA)
  expect_error(arch <- cache$get(file.path(tmp, "archive.rds")), NA)
  expect_equal(pkgs, cache$get(file.path(tmp, "PACKAGES.gz")))

  expect_true(tools::md5sum(file.path(tmp, "PACKAGES.gz")) %in%
              ls(cache$.__enclos_env__$private$data))
  expect_true(tools::md5sum(file.path(tmp, "archive.rds")) %in%
              ls(cache$.__enclos_env__$private$data))
})
