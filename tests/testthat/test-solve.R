
context("solve")

test_that("binary preferred over source", {
  pkgs <- read_fixture("resolution-simple.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_identical(as.logical(sol$solution), c(TRUE, FALSE))
})

test_that("installed preferred over download", {
  pkgs <- read_fixture("resolution-installed.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_identical(as.logical(sol$solution), c(TRUE, FALSE, FALSE))
})
