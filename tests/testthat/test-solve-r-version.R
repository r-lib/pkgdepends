
test_that("R version dependencies", {
  setup_fake_gh_app()
  setup_fake_apps()

  lib <- withr::local_tempdir()
  p1 <- new_pkg_installation_proposal(
    "futurama",
    config = list(library = lib)
  )
  suppressMessages(p1$solve())
  expect_snapshot(
    error = TRUE,
    p1$stop_for_solution_error()
  )

  p2 <- new_pkg_installation_proposal(
    "needsfuturama",
    config = list(library = lib)
  )
  suppressMessages(p2$solve())
  expect_snapshot(
    error = TRUE,
    p2$stop_for_solution_error()
  )
})
