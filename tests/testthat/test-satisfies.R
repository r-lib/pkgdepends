
context("satisfies_remote")

test_that("CRAN packages", {

  ## Satisfies itself
  pkgs <- make_fake_resolution(
    `cran::pkgA` = list()
  )
  expect_true(satisfies_remote(pkgs$resolution[[1]], pkgs$resolution[[1]]))

  ## A certain version satisfies a ref without version requirements
  pkgs <- make_fake_resolution(
    `cran::pkgA` = list(),
    `cran::pkgA@2.0.0` = list()
  )
  expect_true(satisfies_remote(pkgs$resolution[[1]], pkgs$resolution[[2]]))
})
