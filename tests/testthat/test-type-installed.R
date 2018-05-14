
context("installed ref type")

test_that("resolve", {

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)

  tt <- dirname(dirname(attr(packageDescription("testthat"), "file")))
  ref <- paste0("installed::", tt)
  res <- synchronise(
    resolve_remote_installed(parse_remotes(ref)[[1]], TRUE, conf, cache,
                             dependencies = "Imports")
  )

  expect_equal(
    res[c("ref", "type", "direct", "status", "package", "version")],
    list(ref = ref, type = "installed", direct = TRUE, status = "OK",
         package = "testthat",
         version = as.character(packageVersion("testthat")))
  )

  expect_true("crayon" %in% res$unknown_deps)

  expect_equal(res$extra$description$get_field("Package"), "testthat")
})

test_that("download", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

  tt <- dirname(dirname(attr(packageDescription("testthat"), "file")))
  ref <- paste0("installed::", tt)
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  expect_error(r$resolve(), NA)
  expect_error(r$download_resolution(), NA)
  dl <- r$get_resolution_download()
  expect_equal(dl$download_status, "Had")
})

test_that("satisfy", {
  ## Always TRUE, independently of arguments
  expect_true(satisfy_remote_installed())
})
