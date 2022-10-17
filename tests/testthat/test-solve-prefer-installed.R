
test_that("installed preferred over download", {
  setup_fake_apps()

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg2",
    config = list(dependencies = TRUE, library = lib)
  ))
  suppressMessages(p$solve())
  suppressMessages(p$download())
  suppressMessages(p$install())

  p2 <- suppressMessages(new_pkg_installation_proposal(
    "pkg2",
    config = list(dependencies = TRUE, library = lib)
  ))
  suppressMessages(p2$solve())
  sol <- p2$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("type", "package", "version")])
})
