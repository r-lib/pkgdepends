
test_that("pkg_dep_types_*", {
  expect_snapshot(pkg_dep_types_hard())
  expect_snapshot(pkg_dep_types_soft())
  expect_snapshot(pkg_dep_types())
})

test_that("make_null_deps", {
  expect_snapshot(make_null_deps())
})

test_that("parse_deps", {
  # edge cases
  expect_snapshot(parse_deps(character(), character()))
  expect_snapshot(parse_deps("foo", "Imports"))

  # vectorized
  expect_snapshot(parse_deps(c("foo", "foo"), c("Imports", "LinkingTo")))

  # requirement
  expect_snapshot(parse_deps("foo (>= 1.0.0)", "Imports"))

  # multiple packages
  expect_snapshot(parse_deps("foo (>= 1.0.0), bar", "Imports"))

  # newlines are fine
  expect_snapshot(parse_deps("foo (>= 1.0.0)\n bar", "Imports"))
  expect_snapshot(parse_deps("foo (>= \n 1.0.0)\n bar", "Imports"))

  # various relations
  expect_snapshot(parse_deps("R (== 4.2.1)", "Depends"))
  expect_snapshot(parse_deps("R (< 4.2.1)", "Depends"))
  expect_snapshot(parse_deps("R (> 4.2.1)", "Depends"))
  expect_snapshot(parse_deps("R (<= 4.2.1)", "Depends"))

  # base packages are not included
  expect_snapshot(parse_deps("foo, stats", "Imports"))
  expect_snapshot(parse_deps("grid, stats", "Imports"))
})

test_that("parse_all_deps", {
  # edge cases
  expect_snapshot(parse_all_deps(c(Imports = NA_character_)))
  expect_snapshot(parse_all_deps(c(Imports = "grid")))

  expect_snapshot(parse_all_deps(c(
    Imports = "foo (>= 1.0.0), bar",
    Suggests = "baz, foobaz (>= 2.0.0)"
  )))
})

test_that("resolve_ref_deps", {
  dsc <- desc::desc(text = c(
    "Suggests: covr, jsonlite, testthat (>= 3.1.0)",
    "Imports: assertthat, curl, R6, rlang (>= 1.0.0)",
    "Remotes: r-lib/covr",
    "Config/Needs/website: pkgdown, r-lib/downlit"
  ))
  expect_snapshot(resolve_ref_deps(dsc$get_deps(), NA_character_, NULL))
  expect_snapshot(resolve_ref_deps(
    dsc$get_deps(),
    dsc$get("Remotes"),
    dsc$get(extra_config_fields(dsc$fields()))
  ))
})

test_that("resolve_ref_deps, package name from remote", {
  dsc <- desc::desc(text = c(
    "Imports: foo",
    "Remotes: foo=r-lib/bar",
    "Config/Needs/website: foobar=r-lib/baz"
  ))
  expect_snapshot(resolve_ref_deps(
    dsc$get_deps(),
    dsc$get("Remotes"),
    dsc$get(extra_config_fields(dsc$fields()))
  ))

  dsc <- desc::desc(text = c(
    "Imports: foo",
    "Remotes: url::http://example.com"
  ))
  expect_snapshot(
    error = TRUE,
    resolve_ref_deps(
      dsc$get_deps(),
      dsc$get("Remotes"),
      NULL
    )
  )
})

test_that("as_pkg_dependencies", {
  expect_snapshot(as_pkg_dependencies(TRUE))
  expect_snapshot(as_pkg_dependencies("all"))
  expect_snapshot(as_pkg_dependencies("hard"))
  expect_snapshot(as_pkg_dependencies(FALSE))
  expect_snapshot(as_pkg_dependencies(NA))
  expect_snapshot(as_pkg_dependencies(
    list(direct = "Imports", indirect = c("Imports", "Suggests"))
  ))
  expect_snapshot(as_pkg_dependencies(c("Depends", "Imports", "LinkingTo")))
  expect_snapshot(as_pkg_dependencies(
    list(direct = "all", indirect = "hard")
  ))
  expect_snapshot(as_pkg_dependencies(
    list(direct = "all", indirect = "all")
  ))
  expect_snapshot(as_pkg_dependencies(
    list(direct = "all", indirect = c("hard", "soft"))
  ))
})
