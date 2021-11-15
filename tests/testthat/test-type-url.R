
test_that("url remote basics", {
  skip_if_offline()
  skip_on_cran()

  url <- "https://cloud.r-project.org/src/contrib/Archive/zip/zip_2.1.0.tar.gz"
  r <- new_pkg_installation_proposal(
    paste0("url::", url),
    config = list(library = tempfile())
  )
  expect_warning(r$resolve(), NA)
  expect_warning(r$solve(), NA)
  expect_warning(r$download(), NA)
})
