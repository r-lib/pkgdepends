test_that("build_package", {
  args <- NULL
  mockery::stub(
    build_package,
    "pkgbuild::build",
    function(...) args <<- list(...)
  )
  build_package(tmp <- tempfile())
  expect_equal(args$path, tmp)
})

test_that("vignettes can be turned on and off", {
  setup_fake_apps()
  local_cli_config()
  dir.create(tmplib <- tempfile())
  on.exit(rimraf(tmplib), add = TRUE)
  pkgdir <- test_path("fixtures", "packages", "vignettes")
  inst <- new_pkg_installation_proposal(
    paste0("local::", pkgdir, "?nocache"),
    config = list(`build-vignettes` = FALSE, library = tmplib)
  )
  expect_snapshot({
    inst$solve()
    inst$download()
    inst$install()
  })

  expect_false("doc" %in% dir(file.path(tmplib, "pkgdependstest")))
  rimraf(tmplib, "pkgdependstest")

  if (Sys.which("pandoc") == "") skip("Needs pandoc")

  inst2 <- new_pkg_installation_proposal(
    paste0("local::", pkgdir, "?nocache"),
    config = list(`build-vignettes` = TRUE, library = tmplib)
  )

  expect_snapshot({
    inst2$solve()
    inst2$download()
    inst2$install()
  })

  expect_true("doc" %in% dir(file.path(tmplib, "pkgdependstest")))
})
