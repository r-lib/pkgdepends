
context("solve")

test_that("binary preferred over source", {
  pkgs <- read_fixture("resolution-simple.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution)[1:2], c(TRUE, FALSE))
})

test_that("installed preferred over download", {
  pkgs <- read_fixture("resolution-installed.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution[1:3]), c(TRUE, FALSE, FALSE))
})

test_that("dependency versions are honored", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(),
    `pkgA@2.0.0` = list(version = "2.0.0"),
    pkgB = list(deps = make_fake_deps(Imports = "pkgA (>= 2.0.0)"))
  )

  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution[1:3]), c(FALSE, TRUE, TRUE))
})

test_that("conflict: different versions required for package", {
  pkgs <- read_fixture("resolution-gh-vs-cran.rds")
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)

  solution <- list(packages = NULL, result = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)

  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(direct = TRUE),
    `github::user/pkgA` = list(direct = TRUE)
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
})

test_that("standard direct & github indirect is OK", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(direct = TRUE),
    `pkgB` = list(
      direct = TRUE,
      deps = make_fake_deps(Imports = "pkgA", Remotes = "user/pkgA")),
    `user/pkgA` = list()
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution[1:3]), c(FALSE, TRUE, TRUE))
})

test_that("conflict between direct and indirect ref", {
  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(direct = TRUE),
    `pkgB` = list(
      direct = TRUE,
      deps = make_fake_deps(Imports = "pkgA", Remotes = "user/pkgA")),
    `user/pkgA` = list()
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE, FALSE, TRUE, FALSE))
})
