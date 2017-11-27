
context("solve")

test_that("binary preferred over source", {
  pkgs <- read_fixture("resolution-simple.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution), c(TRUE, FALSE))
})

test_that("installed preferred over download", {
  pkgs <- read_fixture("resolution-installed.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution), c(TRUE, FALSE, FALSE))
})

test_that("dependency versions are honored", {
  pkgs <- make_fake_resolution(
    `pkgA@1.0.0` = list(version = "1.0.0"),
    `pkgA@2.0.0` = list(version = "2.0.0"),
    pkgB = list(deps = make_fake_deps(Imports = "pkgA (>= 2.0.0)"))
  )

  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution), c(FALSE, TRUE, TRUE))
})

test_that("conflict: different versions required for package", {
  pkgs <- read_fixture("resolution-gh-vs-cran.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 2)
})
