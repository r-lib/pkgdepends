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
  expect_true(
    lp$obj[which(pkgs$platform != "source")] <
      lp$obj[which(pkgs$platform == "source")]
  )
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
    c(
      rep("exactly-once", length(lp$direct_packages)),
      rep("at-most-once", length(lp$indirect_packages))
    )
  )
  prvar <- which(pkgs$package == "progress")
  expect_equal(
    lp$conds[[1]],
    structure(
      list(
        vars = c(prvar, nrow(pkgs) + 1L),
        coef = c(1, 1, 1),
        op = "==",
        rhs = 1,
        type = "exactly-once",
        note = NULL
      )
    )
  )
  prvar2 <- which(pkgs$package == "assertthat")
  expect_equal(
    lp$conds[[2]],
    structure(list(
      vars = prvar2,
      coef = c(1, 1),
      op = "<=",
      rhs = 1,
      type = "at-most-once",
      note = NULL
    ))
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

test_that("pkgplan_i_lp_prefer_new_binaries skips version-pinned direct refs", {
  # A newer binary (pkg@4.0.2) is available, but the user directly requested
  # pkg@3.5.2 (as a PPM binary with empty platform string). The 3.5.2 candidate
  # must NOT be ruled out by prefer_new_binaries;
  pkgs <- res_make_empty_df()
  for (entry in list(
    make_fake_resolution1("pkg@3.5.2", list(
      direct   = TRUE,
      version  = "3.5.2",
      platform = ""   # PPM binary: empty platform for pure-R packages
    )),
    make_fake_resolution1("pkg", list(
      direct   = FALSE,
      version  = "4.0.2",
      platform = "x86_64-pc-linux-gnu"
    )),
    make_fake_resolution1("pkg1", list(
      direct   = FALSE,
      version  = "1.0.0",
      platform = "x86_64-pc-linux-gnu"
    )),
    make_fake_resolution1("pkg1", list(
      direct   = FALSE,
      version  = "2.0.0",
      platform = "x86_64-pc-linux-gnu"
    ))
  )) pkgs <- res_add_df_entries(pkgs, entry)

  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, "lazy")
  lp <- pkgplan_i_lp_prefer_new_binaries(lp)

  types    <- vcapply(lp$conds, "[[", "type")
  pnb_vars <- unlist(lapply(lp$conds[types == "prefer-new-binary"], "[[", "vars"))

  pkg352_idx   <- which(pkgs$package == "pkg" & pkgs$version == "3.5.2")
  pkg402_idx   <- which(pkgs$package == "pkg" & pkgs$version == "4.0.2")
  pkg1_old_idx <- which(pkgs$package == "pkg1" & pkgs$version == "1.0.0")
  pkg1_new_idx <- which(pkgs$package == "pkg1" & pkgs$version == "2.0.0")

  # The pinned pkg@3.5.2 must not be ruled out
  expect_false(pkg352_idx %in% pnb_vars)
  expect_false(pkg352_idx %in% lp$ruled_out)

  # pkg@4.0.2 is also unaffected (pkg as a whole is skipped when pinned)
  expect_false(pkg402_idx %in% pnb_vars)

  # pkg1@1.0.0 (no pin, indirect) is correctly ruled out in favour of 2.0.0
  expect_true(pkg1_old_idx %in% pnb_vars)
  expect_true(pkg1_old_idx %in% lp$ruled_out)
  expect_false(pkg1_new_idx %in% pnb_vars)
  expect_false(pkg1_new_idx %in% lp$ruled_out)
})

test_that("pkgplan_i_lp_dependencies", {
  pkgs <- read_fixture("resolution-progress.rds")
  config <- current_config()
  lp <- pkgplan_i_lp_init(pkgs, config, "lazy")
  lp <- pkgplan_i_lp_dependencies(lp, config)
  expect_equal(length(lp$conds), 32)
  expect_snapshot(lp$conds[[3]])
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
      c("2.3.4", "2.3.4", "2.3.4", "2.3.4"),
      c("2.3.5", "2.3.2", "2.4.0", "3.0.0"),
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
