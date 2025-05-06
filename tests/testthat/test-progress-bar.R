test_that("should_show_progress_bar", {
  withr::local_options(pkg.show_progress = TRUE)
  expect_true(should_show_progress_bar())

  withr::local_options(pkg.show_progress = FALSE)
  expect_false(should_show_progress_bar())

  withr::local_options(pkg.show_progress = NULL)
  withr::local_envvar(CI = "true")
  expect_false(should_show_progress_bar())

  withr::local_envvar(CI = NA_character_)
  withr::local_options(knitr.in.progress = TRUE)
  expect_false(should_show_progress_bar())

  withr::local_options(knitr.in.progress = NULL)
  withr::local_envvar(TESTTHAT = "true")
  expect_false(should_show_progress_bar())

  withr::local_envvar(TESTTHAT = NA_character_)
  mockery::stub(should_show_progress_bar, "cli::is_dynamic_tty", TRUE)
  expect_true(should_show_progress_bar())
})
