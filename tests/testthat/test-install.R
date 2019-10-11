
context("install_packages")

describe("install_packages", {

  skip_if_offline()

  it("works with source packages", {

  pkg <- source_test_package("foo")

  libpath <- test_temp_dir()

  withr::with_options(list(pkg.show_progress = FALSE), {
    plan <- make_install_plan(
      paste0("local::", pkg), lib = libpath)
    expect_error_free(
      install_package_plan(plan, lib = libpath))
  })

  expect_error_free(
    callr::r(function(l) library("foo", lib.loc = l), list(libpath)))
  })
})
