test_that("dependencies", {
  pkgs <- "
    Package: pkg
    Version: 1.0.0
    Imports: pi
    Depends: pd
    Suggests: ps
    Enhances: pe
    LinkingTo: pl

    Package: pi
    Depends: pid
    Imports: pii
    Suggests: pis
    Enhances: pie
    LinkingTo: pil

    Package: pd
    Depends: pdd
    Imports: pdi
    Suggests: pds
    Enhances: pde
    LinkingTo: pdl

    Package: ps
    Depends: psd
    Imports: psi
    Suggests: pss
    Enhances: pse
    LinkingTo: psl

    Package: pe
    Depends: ped
    Imports: pei
    Suggests: pes
    Enhances: pee
    LinkingTo: pel

    Package: pl
    Depends: pld
    Imports: pli
    Suggests: pls
    Enhances: ple
    LinkingTo: pll"

  oth <- apply(
    expand.grid("p", c("i", "d", "s", "e", "l"), c("i", "d", "s", "e", "l")),
    1,
    paste,
    collapse = ""
  )
  apkgs <- paste0(c(pkgs, sprintf("    Package: %s", oth)), collapse = "\n\n")
  repo <- dcf(apkgs)

  setup_fake_apps(cran_repo = repo)

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(dependencies = NA, library = tempfile())
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  expect_snapshot(p$draw(), transform = transform_bytes)

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(dependencies = TRUE, library = tempfile())
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  expect_snapshot(p$draw(), transform = transform_bytes)

  # We can restrict dependencies. We need to test this more, to
  # make sure that the install plan gets the right dependencies.
  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(dependencies = "LinkingTo", library = tempfile())
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  expect_snapshot(p$draw(), transform = transform_bytes)

  suppressMessages(p$download())
  plan <- p$get_install_plan()
  expect_snapshot({
    plan$package
    plan$dependencies
  })
})

test_that("self dependencies are OK", {
  lib <- tempfile()
  lock <- tempfile()
  on.exit(unlink(c(lib, lock), recursive = TRUE), add = TRUE)

  repo <- dcf(
    "
    Package: pkg
    Suggests: pkg
  "
  )

  setup_fake_apps(cran_repo = repo)

  p <- suppressMessages(new_pkg_installation_proposal(
    "pkg",
    config = list(
      dependencies = TRUE,
      library = lib
    )
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  sol <- p$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_equal(sol$package, "pkg")
  p$create_lockfile(lock)

  plan <- new_pkg_installation_plan(lockfile = lock)
  expect_equal(
    plan$get_solution()$data$dependencies,
    list(character())
  )
})

test_that("circular soft-dependencies are OK", {
  lib <- tempfile()
  lock <- tempfile()
  on.exit(unlink(c(lib, lock), recursive = TRUE), add = TRUE)

  repo <- dcf(
    "
    Package: pkg1
    Suggests: pkg2, pkg3

    Package: pkg2
    Suggests: pkg1

    Package: pkg3
  "
  )

  setup_fake_apps(cran_repo = repo)

  p <- suppressMessages(new_pkg_installation_proposal(
    c("pkg1", "pkg2"),
    config = list(
      dependencies = TRUE,
      library = lib
    )
  ))
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  sol <- p$get_solution()$data
  sol <- sol[order(sol$ref), ]
  expect_equal(sort(sol$package), c("pkg1", "pkg2", "pkg3"))
  p$create_lockfile(lock)

  plan <- new_pkg_installation_plan(lockfile = lock)
  expect_equal(
    plan$get_solution()$data$dependencies,
    list(character(), character(), character())
  )
})
