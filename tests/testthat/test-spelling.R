
test_that("spell check", {
  skip_on_cran()
  skip_in_covr()
  if (utils::packageVersion("spelling") <= "2.1") skip("Needs newer spelling package")
  pkg_dir <- test_package_root()
  suppressMessages(results <- spelling::spell_check_package(pkg_dir))

  if (nrow(results)) {
    output <- sprintf(
      "Potential spelling errors: %s\n",
      paste(results$word, collapse = ", "))
    stop(output, call. = FALSE)
  } else {
    expect_true(TRUE)
  }
})
