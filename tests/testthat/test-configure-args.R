test_that("configure_args / configure_vars reach the install plan", {
  skip_on_cran()

  repo <- dcf(
    "
    Package: pkg
    Version: 1.0.0

    Package: pkg2
    Version: 1.0.0"
  )
  setup_fake_apps(cran_repo = repo)

  prop <- suppressMessages(new_pkg_installation_proposal(
    c("pkg", "pkg2"),
    config = list(
      library = tempfile(),
      configure_args = c(pkg = "--with-foo=/opt/foo"),
      configure_vars = "CC=clang"
    )
  ))
  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  suppressMessages(prop$download())
  plan <- prop$get_install_plan()

  ia <- stats::setNames(plan$install_args, plan$package)

  # `pkg` gets both the per-package configure args and the global vars
  expect_true(
    paste0("--configure-args=", shQuote("--with-foo=/opt/foo")) %in%
      ia[["pkg"]]
  )
  expect_true(
    paste0("--configure-vars=", shQuote("CC=clang")) %in% ia[["pkg"]]
  )

  # `pkg2` only gets the global (unnamed) vars, not `pkg`'s configure args
  expect_false(any(grepl("--configure-args", ia[["pkg2"]])))
  expect_true(
    paste0("--configure-vars=", shQuote("CC=clang")) %in% ia[["pkg2"]]
  )
})

test_that("no configure args when none are configured", {
  skip_on_cran()

  repo <- dcf(
    "
    Package: pkg
    Version: 1.0.0"
  )
  setup_fake_apps(cran_repo = repo)

  prop <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(library = tempfile())
  ))
  suppressMessages(prop$resolve())
  suppressMessages(prop$solve())
  suppressMessages(prop$download())
  plan <- prop$get_install_plan()

  expect_false(any(grepl("--configure-", unlist(plan$install_args))))
})
