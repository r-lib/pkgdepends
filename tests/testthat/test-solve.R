
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
  expect_equal(dsc$failures$type, c("exactly-once", rep("satisfy-refs", 4)))

  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(direct = TRUE),
    `github::user/pkgA` = list(direct = TRUE)
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)

  solution <- list(packages = NULL, result = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failures$type, c("exactly-once", rep("satisfy-refs", 2)))
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

test_that("version conflict", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(version = "1.0.0"),
    `pkgB` = list(
      direct = TRUE,
      deps = make_fake_deps(Imports = "pkgA (>= 2.0.0)"))
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)

  solution <- list(packages = NULL, result = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failures$type, c("exactly-once", "dependency-version"))

  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE, TRUE, FALSE))
})

test_that("resolution failure", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(status = "FAILED")
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)

  solution <- list(packages = NULL, result = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failures$type, c("exactly-once", "ok-resolution"))

  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE))
})

test_that("integration test", {
  skip_on_cran()
  skip_if_offline()

  mkdirp(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  r <- remotes$new(c("r-lib/cli"), lib = lib)
  r$resolve()
  sol <- r$solve()
  expect_true("r-lib/cli" %in% sol$ref)

  r <- remotes$new("cran::cli", lib = lib)
  r$resolve()
  sol <- r$solve()
  expect_true("cran::cli" %in% sol$ref)
  plan <- r$get_install_plan()
  expect_true("cli" %in% plan$package)

  r <- remotes$new(c("cran::cli", "r-lib/cli"), lib = lib)
  r$resolve()
  sol <- r$solve()
  expect_s3_class(sol, "remote_solution_error")
  expect_true("cli" %in% sol$failures$package)
  expect_output(print(sol), "Cannot install package .*cli")
})
