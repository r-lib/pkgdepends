
test_that("pkg_name_check", {
  ## TODO
})

test_that("async_pkg_name_check", {
  ## TODO
})

test_that("format.pkg_name_check", {
  ## TODO
})

test_that("print.pkg_name_check", {
  ## TODO
})

test_that("async_pnc_basics", {
  ## TODO
})

test_that("forbidden_package_names", {
  ## TODO
})

test_that("pnc_valid", {
  good <- c("A3", "aa", "a.3", "foo.bar.foobar")
  bad <- c("3aaa", "a.", "aaaa.", "aaa-bbb", "description", "ff\u00e1f")
  for (c in good) expect_true(pnc_valid(c))
  for (c in bad) expect_false(pnc_valid(c))
})

test_that("pnc_base", {
  local_edition(3)
  local_reproducible_output()
  expect_snapshot(pnc_base("base"))
  expect_snapshot(pnc_base("Base"))
  expect_snapshot(pnc_base("TOOLS"))
  expect_snapshot(pnc_base("definitely-not"))
})

test_that("async_cranlike_check", {
  ## TODO: need a CRAN dummy for this
  expect_true(TRUE)
})

test_that("format.pkg_name_basics", {
  local_edition(3)
  local_reproducible_output()
  bss <- fixture$get({
    list(
      sy(async_pnc_basics("pwr")),
      sy(async_pnc_basics(paste0("s", "h", "i", "t")))
    )
  })
  expect_snapshot_output({
    writeLines(format(bss[[1]]))
    writeLines(format(bss[[2]]))
  })
})

test_that("crandb_check", {
  if (Sys.getenv("PKG_NAME_CHECK_REAL") == "") {
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

test_that("format.pkg_name_check_wikipedia", {
  local_edition(3)
  local_reproducible_output()
  wpd <- fixture$get({
    list(
      sy(async_wikipedia_get("cli")),
      sy(async_wikipedia_get("surely-not-this"))
    )
  })
  expect_snapshot_output({
    writeLines(format(wpd[[1]]))
    writeLines(format(wpd[[2]]))
  })
})

test_that("wikipedia request", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(PKG_NAME_CHECK_WIKIPEDIA_URL = check_app$url("/echo"))
  ret <- synchronize(async_wikipedia_get_query("foobar"))
  expect_snapshot(show_request(ret))
})

test_that("async_wiktionary_get", {

})

test_that("async_wiktionary_get_query", {

})

test_that("pnc_bioc_process", {
  response <- fixture$get({
    sy(pnc_bioc_query("all"))
  })
  ans <- pnc_bioc_process("all", response)
  expect_s3_class(ans, "pkg_name_check_bioc")
  expect_false(ans$bioc)
  expect_equal(ans$package, "ALL")
})
