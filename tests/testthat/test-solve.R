
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
    `pkgA` = list(),
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

  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(direct = TRUE),
    `github::user/pkgA` = list(direct = TRUE)
  )
  lp <- remotes_i_create_lp_problem(pkgs)
  sol <- remotes_i_solve_lp_problem(lp)
  expect_equal(sol$status, 2)
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
  expect_identical(as.logical(sol$solution), c(FALSE, TRUE, TRUE))
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
  expect_equal(sol$status, 2)
})
