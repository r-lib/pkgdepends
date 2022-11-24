
test_that("pkgplan_i_create_lp_init", {
  pkgs <- read_fixture("resolution-simple.rds")
  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, policy = "lazy")
  expect_equal(lp$num_candidates, 2)
  expect_equal(lp$num_direct, 1)
  expect_equal(lp$total, 3)
  expect_equal(lp$conds, list())
  expect_equal(lp$pkgs, pkgs)
  expect_equal(lp$policy, "lazy")
  expect_equal(lp$packages, "pkgconfig")
  expect_equal(lp$direct_packages, "pkgconfig")
  expect_equal(lp$indirect_packages, character())
  expect_equal(lp$ruled_out, integer())
})

test_that("pkgplan_i_lp_objectives lazy policy", {
  pkgs <- read_fixture("resolution-simple.rds")
  config <- current_config()
  lp0 <- pkgplan_i_lp_init(pkgs, config, policy = "lazy")
  lp <- pkgplan_i_lp_objectives(lp0)
  expect_equal(lp0[setdiff(names(lp0), "obj")], lp[setdiff(names(lp), "obj")])
  expect_true(lp$obj[which(pkgs$platform != "source")] <
              lp$obj[which(pkgs$platform == "source")])
  expect_equal(lp$obj[3], solve_dummy_obj)
})

test_that("pkgplan_i_lp_objectives upgrade policy", {
  ## TODO
})

test_that("pkgplan_i_lp_no_multiples", {
  pkgs <- read_fixture("resolution-progress.rds")
  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, "lazy")
  lp <- pkgplan_i_lp_no_multiples(lp)
  expect_equal(
    vcapply(lp$conds, "[[", "type"),
    c(rep("exactly-once", length(lp$direct_packages)),
      rep("at-most-once", length(lp$indirect_packages)))
  )
  prvar <- which(pkgs$package == "progress")
  expect_equal(
    lp$conds[[1]],
    structure(
      list(vars = c(prvar, nrow(pkgs) + 1L), coef = c(1, 1, 1), op = "==",
           rhs = 1, type = "exactly-once", note = NULL))
  )
  prvar2 <- which(pkgs$package == "assertthat")
  expect_equal(
    lp$conds[[2]],
    structure(list(vars = prvar2, coef = c(1, 1), op = "<=", rhs = 1,
                   type = "at-most-once", note = NULL))
  )
})

test_that("pkgplan_i_lp_satisfy_direct", {
  pkgs <- read_fixture("resolution-gh-vs-cran.rds")
  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, "lazy")
  lp <- pkgplan_i_lp_satisfy_direct(lp)
  expect_equal(
    vcapply(lp$conds, "[[", "type"),
    rep("satisfy-refs", 4)
  )
  expect_equal(
    sort(viapply(lp$conds, "[[", "vars")),
    c(1, 2, 3, 3)
  )
  expect_equal(vcapply(lp$conds, "[[", "op"), rep("==", 4))
  expect_equal(vdapply(lp$conds, "[[", "rhs"), rep(0, 4))
})

test_that("pkgplan_i_lp_failures", {
  ## TODO
})

test_that("pkgplan_i_lp_prefer_installed", {
  ## TODO
})

test_that("pkgplan_i_lp_prefer_binaries", {
  ## TODO
})

test_that("pkgplan_i_lp_dependencies", {
  pkgs <- read_fixture("resolution-progress.rds")
  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, "lazy")
  lp <- pkgplan_i_lp_dependencies(lp)
  expect_equal(length(lp$conds), 32)
  expect_equal(
    cli::hash_obj_md5(lp$conds[[3]]),
    "e5ef8d21c1027969a7f4dab12f1561f1"
  )
})

test_that("pkgplan_i_lp_rversion", {
  pkgs <- read_fixture("resolution-progress.rds")
  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, "lazy")
  lp <- pkgplan_i_lp_rversion(lp, "3.1.3")
  fmt <- format(lp)
  expect_true(any(grepl("`rlang` needs a newer R version", fmt, fixed = TRUE)))
  expect_true(any(grepl("`vctrs` needs a newer R version", fmt, fixed = TRUE)))
})

test_that("highlight_version", {
  b <- cli::style_bold
  g <- function(x, .envir = parent.frame()) {
    unname(vcapply(x, glue::glue, .envir = .envir))
  }

  cases <- list(
    list(
      c("2.3.4",      "2.3.4",      "2.3.4",        "2.3.4"),
      c("2.3.5",      "2.3.2",      "2.4.0",        "3.0.0"),
      c("2.3.{b(5)}", "2.3.{b(2)}", "2.{b('4.0')}", "{b('3.0.0')}")
    ),
    list(character(), character(), character()),
    list("1.0.0", "1.0.0", "1.0.0"),
    list(
      c("1.0.0", "1.0.0"),
      c("2.0.0", "1.0.0"),
      c("{b('2.0.0')}", "1.0.0")
    ),
    list("1.0.0.9000", "1.0.0", "1.0.0")
  )

  for (case in cases) {
    expect_equal(highlight_version(case[[1]], case[[2]]), g(case[[3]]))
  }
})
