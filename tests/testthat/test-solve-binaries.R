
test_that("binary preferred over source", {
  repo <- dcf("
    Package: pkg
    Imports: pkg2

    Package: pkg2
  ")

  setup_fake_apps(
    cran_repo = repo,
    cran_options = list(platforms = c("windows", "source"))
  )

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(
      dependencies = TRUE,
      library = tempfile(),
      platforms = c("x86_64-w64-mingw32", "source")
    )
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  sol <- p$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("package", "platform")])
})

test_that("but source is used if that version is required", {
  # we still need this to have a Bioc repo
  setup_fake_apps()
  repo1 <- dcf("
    Package: pkg
    Imports: pkg2 (>= 2.0.0)

    Package: pkg2
    Version: 1.0.0
  ")

  repo2 <- dcf("
   Package: pkg2
   Version: 2.0.0
  ")

  fake1 <- webfakes::new_app_process(
    cran_app(repo1, options = list(platforms = c("windows", "source")))
  )
  fake2 <- webfakes::new_app_process(
    cran_app(repo2, options = list(platforms = "source"))
  )
  withr::local_options(repos = c(CRAN = fake1$url(), X = fake2$url()))

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(
      dependencies = TRUE,
      library = tempfile(),
      platforms = c("x86_64-w64-mingw32", "source")
    )
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  sol <- p$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("package", "version", "platform")])
})
