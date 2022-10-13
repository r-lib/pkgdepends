
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
    transform = function(x) sub("[(][0-9]+ B[)]", "(<size>)", x)
  )
})
