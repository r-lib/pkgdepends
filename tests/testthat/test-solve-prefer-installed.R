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

  # multiple libraries
  lib2 <- tempfile()
  on.exit(unlink(lib2, recursive = TRUE), add = TRUE)

  p3 <- suppressMessages(new_pkg_installation_proposal(
    "pkg2",
    config = list(dependencies = TRUE, library = c(lib2, lib))
  ))
  suppressMessages(p3$solve())
  sol <- p3$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("type", "package", "version")])

  p4 <- suppressMessages(new_pkg_installation_proposal(
    "pkg2",
    config = list(dependencies = TRUE, library = c(lib2))
  ))
  suppressMessages(p4$solve())
  sol <- p4$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_snapshot(sol[, c("type", "package", "version")])
})
