
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
})
