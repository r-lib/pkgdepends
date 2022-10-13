
describe("install_packages", {

  setup_fake_apps()
  local_cli_config()

  it("works with source packages", {

    pkg <- source_test_package("foo")

    libpath <- test_temp_dir()
    
    expect_snapshot({
      plan <- make_install_plan(paste0("local::", pkg, "?nocache"), lib = libpath)
      install_package_plan(plan, lib = libpath)
    })

    callr::r(function(l) library("foo", lib.loc = l), list(libpath))
  })

})

# Test the fix for https://github.com/r-lib/pkgdepends/issues/160
test_that("install_plan dependency order with binary and source pkgs", {
  plan <- structure(list(
    package = c(
      "pkgdepends", "assertthat", "backports",
      "callr", "cli", "crayon", "curl", "desc", "digest", "ellipsis",
      "fansi", "filelock", "glue", "jsonlite", "lpSolve", "pillar",
      "pkgbuild", "pkgcache", "pkgconfig", "prettyunits", "ps", "R6",
      "rappdirs", "rematch2", "rlang", "rprojroot", "tibble", "utf8",
      "uuid", "vctrs", "withr", "zip", "processx"),
    binary = c(
      FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    dependencies = structure(list(
      c("assertthat", "backports", "callr", "cli", "crayon", "curl",
        "desc", "filelock", "glue", "jsonlite", "lpSolve", "pkgbuild",
        "pkgcache", "prettyunits", "ps", "rematch2", "rprojroot", "R6",
        "tibble", "withr", "zip"),
      character(0),
      character(0),
      c("processx", "R6"),
      c("assertthat", "crayon", "glue", "fansi"),
      character(0),
      character(0),
      c("assertthat", "R6", "crayon", "rprojroot"),
      character(0),
      "rlang",
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      c("cli", "crayon", "fansi", "rlang", "utf8", "vctrs"),
      c("callr", "cli", "crayon", "desc", "prettyunits", "R6", "rprojroot",
        "withr"),
      c("assertthat", "callr", "cli", "curl", "digest", "filelock", "glue",
        "prettyunits", "R6", "processx", "rappdirs", "rlang", "tibble",
        "uuid"),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      "tibble",
      character(0),
      "backports",
      c("cli", "crayon", "fansi", "pillar", "pkgconfig", "rlang"),
      character(0),
      character(0),
      c("ellipsis", "digest", "glue", "rlang"),
      character(0),
      character(0),
      c("ps", "R6")
    ), class = "AsIs")),
    metadata = list(
      download_start = structure(
        1583239144.75085,
        class = c("POSIXct", "POSIXt")),
      download_end = structure(
        1583239146.91793,
        class = c("POSIXct", "POSIXt"))
    ),
    row.names = c(NA, -33L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  plan2 <- add_recursive_dependencies(plan)
  expect_equal(plan$binary, plan2$binary)
  expect_equal(
    plan$dependencies[plan$binary],
    plan2$dependencies[plan2$binary]
  )
  expect_equal(
    sort(plan2$dependencies[[1]]),
    sort(c(plan$dependencies[[1]],
      c("processx", "fansi", "digest", "rappdirs", "rlang", "uuid",
        "pillar", "pkgconfig", "utf8", "vctrs", "ellipsis")))
  )
  expect_equal(
    sort(plan$dependencies[[33]]),
    sort(plan2$dependencies[[33]])
  )
})
