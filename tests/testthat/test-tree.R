
cli::test_that_cli("draw_solution_tree", {

  cran_app_pkgs <- dcf("
    Package: pkgconfig
    Version: 2.0.3
    Suggests: covr, testthat, disposables (>= 1.0.3)
    Imports: utils

    Package: dplyr
    Version: 1.0.10
    Imports: generics, glue (>= 1.3.2), lifecycle (>= 1.0.1), magrittr (>= 1.5), methods, R6, rlang (>= 1.0.2), tibble (>= 2.1.3), tidyselect (>= 1.1.1), utils, vctrs (>= 0.4.1), pillar (>= 1.5.1)
    Suggests: bench, broom, callr, covr, DBI, dbplyr (>= 1.4.3), ggplot2, knitr, Lahman, lobstr, microbenchmark, nycflights13, purrr, rmarkdown, RMySQL, RPostgreSQL, RSQLite, testthat (>= 3.1.1), tidyr, withr

    Package: R6
    Version: 2.5.1
    Suggests: testthat, pryr

    Package: cli
    Version: 3.4.1
    Imports: utils
    Suggests: callr, covr, digest, glue (>= 1.6.0), grDevices, htmltools, htmlwidgets, knitr, methods, mockery, processx, ps (>= 1.3.4.9000), rlang (>= 1.0.2.9003), rmarkdown, rprojroot, rstudioapi, testthat, tibble, whoami, withr

    Package: fansi
    Version: 1.0.3
    Imports: grDevices, utils
    Suggests: unitizer, knitr, rmarkdown

    Package: generics
    Version: 0.1.3
    Imports: methods
    Suggests: covr, pkgload, testthat (>= 3.0.0), tibble, withr

    Package: glue
    Version: 1.6.2
    Imports: methods
    Suggests: covr, crayon, DBI, dplyr, forcats, ggplot2, knitr, magrittr, microbenchmark, R.utils, rmarkdown, rprintf, RSQLite, stringr, testthat (>= 3.0.0), vctrs (>= 0.3.0), waldo (>= 0.3.0), withr

    Package: lifecycle
    Version: 1.0.3
    Imports: cli (>= 3.4.0), glue, rlang (>= 1.0.6)
    Suggests: covr, crayon, knitr, lintr, rmarkdown, testthat (>= 3.0.1), tibble, tidyverse, tools, vctrs, withr

    Package: magrittr
    Version: 2.0.3
    Suggests: covr, knitr, rlang, rmarkdown, testthat

    Package: pillar
    Version: 1.8.1
    Imports: cli (>= 2.3.0), fansi, glue, lifecycle, rlang (>= 1.0.2), utf8 (>= 1.1.0), utils, vctrs (>= 0.3.8)
    Suggests: bit64, debugme, DiagrammeR, dplyr, formattable, ggplot2, knitr, lubridate, nanotime, nycflights13, palmerpenguins,  rmarkdown, scales, stringi, survival, testthat (>= 3.1.1), tibble, units (>= 0.7.2), vdiffr, withr

    Package: rlang
    Version: 1.0.6
    Imports: utils
    Suggests: cli (>= 3.1.0), covr, crayon, fs, glue, knitr, magrittr, methods, pillar, rmarkdown, stats, testthat (>= 3.0.0), tibble, usethis, vctrs (>= 0.2.3), withr

    Package: tibble
    Version: 3.1.8
    Imports: fansi (>= 0.4.0), lifecycle (>= 1.0.0), magrittr, methods, pillar (>= 1.7.0), pkgconfig, rlang (>= 1.0.2), utils, vctrs (>= 0.3.8)
    Suggests: bench, bit64, blob, brio, callr, cli, covr, crayon (>= 1.3.4), DiagrammeR, dplyr, evaluate, formattable, ggplot2, hms, htmltools, knitr, lubridate, mockr, nycflights13, pkgbuild, pkgload, purrr, rmarkdown, stringi, testthat (>= 3.0.2), tidyr, withr

    Package: tidyselect
    Version: 1.2.0
    Imports: cli (>= 3.3.0), glue (>= 1.3.0), lifecycle (>= 1.0.3), rlang (>= 1.0.4), vctrs (>= 0.4.1), withr
    Suggests: covr, crayon, dplyr, knitr, magrittr, rmarkdown, stringr, testthat (>= 3.1.1), tibble (>= 2.1.3)

    Package: utf8
    Version: 1.2.2
    Suggests: cli, covr, knitr, rlang, rmarkdown, testthat (>= 3.0.0), withr

    Package: vctrs
    Version: 0.4.2
    Imports: cli (>= 3.2.0), glue, rlang (>= 1.0.2)
    Suggests: bit64, covr, crayon, dplyr (>= 0.8.5), generics, knitr, pillar (>= 1.4.4), pkgdown (>= 2.0.1), rmarkdown, testthat (>= 3.0.0), tibble (>= 3.1.3), withr, xml2, waldo (>= 0.2.0), zeallot

    Package: withr
    Version: 2.5.0
    Imports: graphics, grDevices, stats
    Suggests: callr, covr, DBI, knitr, lattice, methods, rlang, rmarkdown (>= 2.12), RSQLite, testthat (>= 3.0.0)
  ")

  fake_cran <- webfakes::local_app_process(
    cran_app(cran_app_pkgs),
    opts = webfakes::server_opts(num_threads = 3)
  )
  withr::local_options(
    repos = c(CRAN = fake_cran$url()),
    pkg.cran_metadata_url = fake_cran$url(),
    pkg.emoji = FALSE
  )
  withr::local_envvar(
    R_PKG_CRAN_METADATA_URL = fake_cran$url()
  )

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  r <- pkg_plan$new(
    c("pkgconfig", "dplyr"), library = lib,
    config = list(use_bioconductor = FALSE)
  )
  suppressMessages(r$resolve())
  suppressMessages(r$solve())

  expect_snapshot(
    r$draw_solution_tree(),
    transform = transform_bytes
  )
})

test_that("tree from lockfile", {
  setup_fake_apps()
  lib <- withr::local_tempdir()

  # create a lockfile first
  plan <- suppressMessages(new_pkg_installation_proposal(
    "pkg3",
    config = list(library = lib)
  ))
  suppressMessages(plan$resolve())
  plan$solve()
  lockfile <- tempfile(fileext = ".lock")
  on.exit(unlink(lockfile), add = TRUE)
  plan$create_lockfile(lockfile)

  plan <- pkgdepends::new_pkg_installation_plan(
    lockfile,
    config = list(library = lib)
  )
  expect_snapshot(plan$draw(), transform = transform_bytes)
})

test_that("update", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()
  lib <- withr::local_tempdir()

  url <- paste0(
    fake_cran$url(),
    "/src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz"
  )

  config <- list(library = lib)
  plan <- suppressMessages(new_pkg_installation_proposal(
    paste0("url::", url),
    config = config
  ))
  suppressMessages(plan$solve())
  suppressMessages(plan$download())
  suppressMessages(plan$install())

  plan2 <- new_pkg_installation_proposal("pkg1", config = config)
  plan2$solve()
  expect_snapshot(plan2$draw(), transform = transform_bytes)

  plan3 <- new_pkg_installation_proposal("any::pkg1", config = config)
  plan3$solve()
  expect_snapshot(plan3$draw(), transform = transform_bytes)
})

test_that("has_emoji", {
  mockery::stub(has_emoji, "is_utf8_output", FALSE)
  expect_false(has_emoji())

  mockery::stub(has_emoji, "is_utf8_output", TRUE)
  withr::local_options(pkg.emoji = TRUE)
  expect_true(has_emoji())

  withr::local_options(pkg.emoji = FALSE)
  expect_false(has_emoji())

  withr::local_options(pkg.emoji = NULL)
  mockery::stub(has_emoji, "Sys.info", c(sysname = "Linux"))
  expect_false(has_emoji())

  mockery::stub(has_emoji, "Sys.info", c(sysname = "Darwin"))
  expect_true(has_emoji())
})

test_that("no emoji", {
  mockery::stub(emoji, "has_emoji", FALSE)
  mockery::stub(emoji, "emo_builder", "\U1F477")
  expect_snapshot({
    emoji("rocket")
    emoji("sparkles")
    emoji("hand")
    emoji("dl")
    emoji("builder")
    emoji("wrench")
    emoji("pkg")
    emoji("pkgs")
    emoji("foobar")
  })
})

# The rest is for UTF-8 systems only

test_that("emoji", {
  if (! l10n_info()$"UTF-8") skip("Not UTF-8")
  mockery::stub(emoji, "has_emoji", TRUE)
  mockery::stub(emoji, "emo_builder", "\U1F477")
  expect_snapshot({
    emoji("rocket")
    emoji("sparkles")
    emoji("hand")
    emoji("dl")
    emoji("builder")
    emoji("wrench")
    emoji("pkg")
    emoji("pkgs")
    emoji("foobar")
  })
})

test_that("emo_builder", {
  if (! l10n_info()$"UTF-8") skip("Not UTF-8")

  # make it deterministic
  mockery::stub(
    emo_builder,
    "sample",
    function(x, size, replace = FALSE, ...) {
      rep_len(utils::tail(x, size), size)
    }
  )

  mockery::stub(emo_builder, "rstudio_detect", list(type = "rstudio_terminal"))
  expect_snapshot(emo_builder(5))

  mockery::stub(emo_builder, "rstudio_detect", list(type = "rstudio_console"))
  expect_snapshot(emo_builder(5))

  mockery::stub(emo_builder, "rstudio_detect", list(type = "not_rstudio"))
  expect_snapshot(emo_builder(5))
})
