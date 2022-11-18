
test_that("different versions required", {
  setup_fake_gh_app()
  setup_fake_apps()

  lib <- withr::local_tempdir()
  p <- new_pkg_installation_proposal(
    c("r-lib/crayon", "crayon"),
    config = list(library = lib)
  )

  suppressMessages(p$solve())
  expect_snapshot(
    error = TRUE,
    p$stop_for_solution_error(),
    # need to sort this, otherwise it is not deterministic
    transform = function(x) {
      x[-1] <- sort(x[-1], decreasing = TRUE)
      x
    }
  )
})

test_that("direct CRAN conflicts with downstream GH dep", {

  gh <- list(
    users = list(
      "r-lib" = list(
         repos = list(
           foo = list(
             commits = list(
               list(
                 sha = "b69f0b34da3b538a666c02944d67ebae9535488fa9d29a010a79980822b56780",
                 branch = "main",
                 tag = "HEAD",
                 files = list(
                   DESCRIPTION = "Package: foo\nVersion: 1.0.0\nImports: crayon\nRemotes: r-lib/crayon\n",
                   NAMESPACE = ""
                 )
               )
             )
           ),
           crayon = list(
             commits = list(
               list(
                 sha = "09ec8a1bbab45ba6a54f896f721fdf00798559ef71852063ea4934d9656101d0",
                 branch = "main",
                 tag = "HEAD",
                 files = list(
                   DESCRIPTION = gh_app_desc("crayon"),
                   NAMESPACE = ""
                 )
               )
             )
           )
         )
       )
    )
  )

  fake_gh <- webfakes::local_app_process(
    gh_app(gh)
  )
  withr::local_envvar(R_PKG_GITHUB_API_URL = fake_gh$url())
  setup_fake_apps()

  lib <- withr::local_tempdir()
  p <- new_pkg_installation_proposal(
    c("r-lib/foo", "crayon"),
    config = list(library = lib)
  )

  suppressMessages(p$solve())
  expect_snapshot(
    error = TRUE,
    p$stop_for_solution_error()
  )
})

test_that("no required version", {
  repo <- dcf("
    Package: pkg1
    Depends: pkg2 (>= 2.0.0)

    Package: pkg2
    Version: 1.0.0
  ")
  setup_fake_apps(cran_repo = repo)

  lib <- withr::local_tempdir()
  p <- new_pkg_installation_proposal("pkg1", config = list(library = lib))

  suppressMessages(p$solve())
  expect_snapshot(error = TRUE, p$stop_for_solution_error())
})

test_that("failed resolution", {
  setup_fake_apps()
  setup_fake_gh_app()

  lib <- withr::local_tempdir()
  p1 <- new_pkg_installation_proposal("SDF", config = list(library = lib))
  suppressMessages(p1$solve())
  expect_snapshot(error = TRUE, p1$stop_for_solution_error())

  p2 <- new_pkg_installation_proposal("SDF/SDF", config = list(library = lib))
  suppressMessages(p2$solve())
  expect_snapshot(error = TRUE, p2$stop_for_solution_error())
})
