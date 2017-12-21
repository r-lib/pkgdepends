
context("standard ref type")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  ## CRAN package is found
  withr::with_options(c(pkg.progress.bar = FALSE), {
    res <- remotes$new("crayon", config = list(dependencies = FALSE))$
      resolve()
  })
  expect_true(any(res$data[]$status == "OK"))
  expect_equal(
    get_files(res$data$resolution[[1]])[[1]]$metadata[["RemoteType"]],
    "cran"
  )

  ## BioC package is found
  withr::with_options(c(pkg.progress.bar = FALSE), {
    res <- remotes$new("Biobase", config = list(dependencies = FALSE))$
      resolve()
  })
  expect_true(any(res$data[]$status == "OK"))
  expect_equal(
    get_files(res$data$resolution[[1]])[[1]]$metadata[["RemoteType"]],
    "bioc"
  )

  ## Proper error for non-existing package
  nonpkg <- basename(tempfile())
  withr::with_options(c(pkg.progress.bar = FALSE), {
    res <- remotes$new(nonpkg, config = list(dependencies = FALSE))$
      resolve()
  })
  expect_true(all(res$data[]$status == "FAILED"))
})

test_that("download_remote", {

  skip_if_offline()
  skip_on_cran()

  withr::with_options(c(pkg.progress.bar = FALSE), {
    r <- remotes$new("crayon", config = list(dependencies = FALSE))
    r$resolve()
    dl <- r$download_resolution()
  })
  expect_true(all(file.exists(dl$data[]$fulltarget)))
})
