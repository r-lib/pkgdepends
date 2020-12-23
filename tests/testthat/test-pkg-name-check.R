
test_that("pnc_base", {
  local_edition(3)
  expect_snapshot(pnc_base("base"))
  expect_snapshot(pnc_base("Base"))
  expect_snapshot(pnc_base("TOOLS"))
  expect_snapshot(pnc_base("definitely-not"))
})

test_that("crandb_check", {
  if (Sys.getenv("PKG_NAME_CHECK_REAL") != "") {
    withr::local_envvar(PKG_NAME_CHECK_CRANDB_URL = check_app$url("/crandb"))
  }

  res <- synchronize(when_all(
    good = async_pnc_crandb("no-such-package"),
    bad = async_pnc_crandb("dbi"),
    bad2 = async_pnc_crandb("DBI"),
    bad3 = async_pnc_crandb("Dbi")
  ))

  expect_true(res$good$crandb)
  expect_false(res$bad$crandb)
  expect_false(res$bad2$crandb)
  expect_false(res$bad3$crandb)

  expect_null(res$good$package)
  expect_equal(res$bad$package, "DBI")
  expect_equal(res$bad2$package, "DBI")
  expect_equal(res$bad2$package, "DBI")

  lapply(res, expect_s3_class, "pkg_name_check_crandb")
})

test_that("wikipedia_get", {
  withr::local_envvar(PKG_NAME_CHECK_WIKIPEDIA_URL = check_app$url("/wikipedia"))
  ret <- synchronize(async_wikipedia_get("foobar"))
  expect_equal(ret$term, "foobar")
  expect_equal(ret$normalized, "Foobar")
})

test_that("wikipedia request", {
  local_edition(3)
  withr::local_envvar(PKG_NAME_CHECK_WIKIPEDIA_URL = check_app$url("/echo"))
  ret <- synchronize(async_wikipedia_get_query("foobar"))
  expect_snapshot(show_request(ret))
})
