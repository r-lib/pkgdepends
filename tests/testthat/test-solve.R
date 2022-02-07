
test_that("binary preferred over source", {
  pkgs <- read_fixture("resolution-simple.rds")
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution)[1:2], c(TRUE, FALSE))
})

test_that("installed preferred over download", {
  pkgs <- read_fixture("resolution-installed.rds")
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, policy = "lazy", config = config)
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution[1:3]), pkgs$type == "installed")
})

test_that("dependency versions are honored", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(),
    `pkgA@2.0.0` = list(version = "2.0.0"),
    pkgB = list(
      direct = TRUE,
      deps = list(make_fake_deps(Imports = "pkgA (>= 2.0.0)")))
  )

  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_identical(as.logical(sol$solution[1:3]), c(FALSE, TRUE, TRUE))
})

test_that("conflict: different versions required for package", {
  pkgs <- read_fixture("resolution-gh-vs-cran.rds")
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)

  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failure_type, rep("satisfy-direct", 3))

  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(direct = TRUE),
    `github::user/pkgA` = list(direct = TRUE)
  )
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)

  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failure_type, rep("satisfy-direct", 2))
})

test_that("standard direct & github indirect is not OK", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(direct = TRUE),
    `pkgB` = list(
      direct = TRUE,
      deps = list(make_fake_deps(Imports = "pkgA", Remotes = "user/pkgA"))),
    `user/pkgA` = list(extra = list(list(remotesha = "badcafe")))
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sum(sol$solution * sol$objective) > 1e+8)
})

test_that("conflict between direct and indirect ref", {
  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(direct = TRUE),
    `pkgB` = list(
      direct = TRUE,
      deps = list(make_fake_deps(Imports = "pkgA", Remotes = "user/pkgA"))),
    `user/pkgA` = list(list(extra = list(sha = "badcafe")))
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("version conflict", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(version = "1.0.0"),
    `pkgB` = list(
      direct = TRUE,
      deps = list(make_fake_deps(Imports = "pkgA (>= 2.0.0), pkgC"))),
    `pkgC` = list()
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)

  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failure_type, "dep-failed")

  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("resolution failure", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(status = "FAILED", direct = TRUE)
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)

  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failure_type, "failed-res")

  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE))
})

test_that("integration test", {
  skip_on_cran()
  skip_if_offline()

  mkdirp(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  r <- pkg_plan$new(c("r-lib/cli"), lib = lib)
  r$resolve()
  sol <- r$solve()
  expect_true("r-lib/cli" %in% sol$data$ref)

  r <- pkg_plan$new("cran::cli", lib = lib)
  r$resolve()
  sol <- r$solve()
  expect_true("cran::cli" %in% sol$data$ref)
  expect_true("cli" %in% r$get_solution()$data$package)

  r <- pkg_plan$new(c("cran::cli", "r-lib/cli"), lib = lib)
  r$resolve()
  sol <- r$solve()
  expect_equal(sol$status, "FAILED")
  expect_true("cli" %in% sol$failures$package)
  out <- capture_output(print(sol))
  expect_match(crayon::strip_style(out), "cran::cli:")
})

test_that("failure in non-needed package is ignored", {
  pkgs <- make_fake_resolution(
    aa = list(direct = TRUE, deps = list(make_fake_deps(Imports = "bb, xx"))),
    bb = list(status = "FAILED"),
    `bb/bb` = list(),
    xx = list()
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1e4)
  expect_equal(as.logical(sol$solution), as.logical(c(1, 0, 1, 1, 0)))
})

test_that("failure in dependency of a non-needed package is ignored", {
  pkgs <- make_fake_resolution(
    aa = list(direct = TRUE, deps = list(make_fake_deps(Imports = "aa2"))),
    aa2 = list(deps = list(make_fake_deps(Imports = "bb"))),
    bb = list(status = "FAILED"),
    `bb/bb` = list()
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1e4)
  expect_equal(as.logical(sol$solution), as.logical(c(1, 1, 0, 1, 0)))
})

test_that("failure if package needs newer R version", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(
      version = "1.0.0",
      direct = TRUE,
      deps = list(make_fake_deps(Depends = "R (>= 1000.0)"))
    ),
    `pkgB` = list(
      direct = TRUE,
      deps = list(make_fake_deps(Imports = "pkgC"))
    ),
    `pkgC` = list()
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)

  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failure_type, c("old-rversion"))

  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("failure if dependency needs newer R version", {
  pkgs <- make_fake_resolution(
    `pkgA` = list(
      version = "1.0.0",
      deps = list(make_fake_deps(Depends = "R (>= 1000.0)"))
    ),
    `pkgB` = list(
      direct = TRUE,
      deps = list(make_fake_deps(Imports = "pkgA (>= 1.0.0), pkgC"))
    ),
    `pkgC` = list()
  )
  config <- current_config()$set("platforms", c("macos", "source"))
  lp <- pkgplan_i_create_lp_problem(pkgs, config = config, policy = "lazy")
  sol <- pkgplan_i_solve_lp_problem(lp)

  solution <- list(status = "FAILED", data = NULL, problem = lp,
                   solution = sol)
  dsc <- describe_solution_error(pkgs, solution)
  expect_equal(dsc$failure_type, c("old-rversion", "dep-failed"))

  expect_equal(sol$status, 0)
  expect_true(sol$objval >= solve_dummy_obj - 1L)
  expect_equal(as.logical(sol$solution), c(FALSE, FALSE, FALSE, TRUE))
})
