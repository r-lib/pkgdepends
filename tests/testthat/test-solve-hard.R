test_that("hard example, because easch package has many candidates", {
  skip("unfinished")

  lib <- test_temp_dir()
  install.packages(
    c("dplyr", "rcmdcheck", "qgraph", "usethis", "ggplot2", "knitr"),
    lib = lib
  )

  r <- pkg_plan$new(
    c(
      "tidyverse/dplyr",
      "r-lib/rcmdcheck",
      "cran/qgraph",
      "r-lib/usethis",
      "tidyverse/ggplot2",
      "yihui/knitr",
      "r-lib/rray"
    ),
    lib = lib
  )
  r$resolve()

  pkgs <- r$get_resolution()
  prob <- r$.__enclos_env__$private$create_lp_problem(pkgs, "upgrade")
  sol <- pkgplan_i_solve_lp_problem(prob)

  expect_true(sol$objval < 1e6)
})
