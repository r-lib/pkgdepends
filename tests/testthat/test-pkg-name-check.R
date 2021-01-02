
test_that("pkg_name_check", {
  mockery::stub(
    pkg_name_check,
    "async_pkg_name_check",
    function(...) async_constant("boo")
  )
  expect_equal(pkg_name_check("foobar"), "boo")
})

test_that("async_pkg_name_check", {
  expect_error(sy(async_pkg_name_check(11)))
  expect_error(sy(async_pkg_name_check(c("a", "b"))))
  expect_error(sy(async_pkg_name_check(NA_character_)))
  expect_error(sy(async_pkg_name_check("x", 11)))
  expect_error(sy(async_pkg_name_check("x", NA_character_)))

  calls <- 0L
  fun <- function(name) calls <<- calls + 1L
  mockery::stub(async_pkg_name_check, "async_pnc_basics", fun)
  mockery::stub(async_pkg_name_check, "async_wikipedia_get", fun)
  mockery::stub(async_pkg_name_check, "async_wiktionary_get", fun)
  mockery::stub(async_pkg_name_check, "async_acromine_get", fun)
  mockery::stub(async_pkg_name_check, "async_sentiment_get", fun)
  mockery::stub(async_pkg_name_check, "async_urban_get", fun)

  ans <- sy(async_pkg_name_check("foo"))
  expect_s3_class(ans, "pkg_name_check")
  expect_equal(sort(unlist(ans, use.names = FALSE)), 1:5)
})

# format.pkg_name_check via print

test_that("print.pkg_name_check", {
  local_edition(3)
  local_reproducible_output()
  ans <- fixture$get({
    list(
      pkg_name_check("tools"),
      pkg_name_check("tools", dictionaries = "urban")
    )
  })
  expect_snapshot(print(ans[[1]]))
  expect_snapshot(print(ans[[2]]))
})

test_that("async_pnc_basics", {
  mockery::stub(async_pnc_basics, "async_pnc_crandb", "crandb")
  mockery::stub(async_pnc_basics, "async_pnc_bioc", "bioc")
  mockery::stub(async_pnc_basics, "async_profanity_get", "profanity")
  ans <- sy(async_pnc_basics("name"))
  expect_s3_class(ans, "pkg_name_check_basics")
  expect_equal(
    names(ans),
    c("name", "valid", "base", "crandb", "bioc", "profanity")
  )
})

test_that("forbidden_package_names", {
  expect_true(is.character(forbidden_package_names()))
})

test_that("pnc_valid", {
  good <- c("A3", "aa", "a.3", "foo.bar.foobar")
  bad <- c("3aaa", "a.", "aaaa.", "aaa-bbb", "description", "ff\u00e1f")
  for (c in good) expect_true(pnc_valid(c))
  for (c in bad) expect_false(pnc_valid(c))

  mockery::stub(pnc_valid, "grepl", TRUE)
  expect_false(pnc_valid("ff\u00e1f"))
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
  expect_snapshot({
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

test_that("async_wikipedia_get", {
  withr::local_envvar(PKG_NAME_CHECK_WIKIPEDIA_URL = check_app$url("/wikipedia"))
  ret <- synchronize(async_wikipedia_get("foobar"))
  expect_equal(ret$term, "foobar")
  expect_equal(ret$normalized, "Foobar")
})

test_that("async_wikipedia_get_query", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(PKG_NAME_CHECK_WIKIPEDIA_URL = check_app$url("/echo"))
  ret <- synchronize(async_wikipedia_get_query("foobar"))
  expect_snapshot(show_request(ret))
})

# make_wikipedia_data
# wikipedia_get_process

test_that("format.pkg_name_check_wikipedia", {
  local_edition(3)
  local_reproducible_output()
  wpd <- fixture$get({
    list(
      sy(async_wikipedia_get("cli")),
      sy(async_wikipedia_get("surely-not-this")),
      sy(async_wikipedia_get("GNU R"))
    )
  })
  expect_snapshot({
    writeLines(format(wpd[[1]]))
    writeLines(format(wpd[[2]]))
    writeLines(format(wpd[[3]]))
  })
})

# clean_wikipedia_text

test_that("async_wiktionary_get", {
  local_edition(3)
  local_reproducible_output()
  resp <- fixture$get({
    sy(async_wiktionary_get_query("foobar"))
  })
  mockery::stub(
    async_wiktionary_get,
    "async_wiktionary_get_query",
    function(...) async_constant(resp)
  )
  ans <- sy(async_wiktionary_get("foobar"))
  expect_s3_class(ans, "pkg_name_check_wiktionary")
  expect_s3_class(ans, "tbl_df")
  expect_snapshot(print(ans))
})

test_that("async_wiktionary_get_query", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(PKG_NAME_CHECK_WIKTIONARY_URL = check_app$url("/echo"))
  ret <- synchronize(async_wiktionary_get_query("foobar"))
  expect_snapshot(show_request(ret))
})

test_that("wiktionary_get_process", {
  resp <- fixture$get({
    sy(async_wiktionary_get_query("foobar"))
  })
  ans <- wiktionary_get_process("foobar", resp)
  expect_s3_class(ans, "pkg_name_check_wiktionary")
  expect_s3_class(ans, "tbl_df")
  expect_match(ans$text, "== English ==")
})

test_that("format.pkg_name_check_wiktionary", {
  local_edition(3)
  local_reproducible_output()
  resp <- fixture$get({
    sy(async_wiktionary_get_query("foobar"))
  })
  resp2 <- fixture$get({
    sy(async_wiktionary_get_query("not-at-all-sdfsdfsdf"))
  })
  ans <- wiktionary_get_process("foobar", resp)
  ans2 <- wiktionary_get_process("not-at-all-sdfsdfsdf", resp2)
  expect_snapshot({
    writeLines(format(ans))
    writeLines(format(ans2))
  })
})

# clean_wiktionary_text

test_that("async_acromine_get", {
  resp <- fixture$get({
    sy(async_acromine_get_query("fbi"))
  })
  mockery::stub(
    async_acromine_get,
    "async_acromine_get_query",
    function(...) async_constant(resp)
  )
  ans <- sy(async_acromine_get("fbi"))
  expect_s3_class(ans, "pkg_name_check_acromine")
  expect_s3_class(ans, "tbl_df")
  expect_true("Federal Bureau of Investigation" %in% ans$long_form)
})

test_that("async_acromine_get_query", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(PKG_NAME_CHECK_ACROMINE_URL = check_app$url("/echo"))
  ret <- synchronize(async_acromine_get_query("foobar"))
  expect_snapshot(show_request(ret))
})

test_that("acromine_get_process", {
  resp <- fixture$get({
    sy(async_acromine_get_query("fbi"))
  })
  resp2 <- fixture$get({
    sy(async_acromine_get_query("notanacronym"))
  })
  ans <- acromine_get_process("fbi", resp)
  expect_s3_class(ans, "pkg_name_check_acromine")
  expect_s3_class(ans, "tbl_df")
  expect_true("Federal Bureau of Investigation" %in% ans$long_form)

  ans2 <- acromine_get_process("notanacronym", resp2)
  expect_s3_class(ans2, "pkg_name_check_acromine")
  expect_s3_class(ans2, "tbl_df")
  expect_equal(nrow(ans2), 0L)
})

test_that("format.pkg_name_check_acromine", {
  local_edition(3)
  local_reproducible_output()
  resp <- fixture$get({
    sy(async_acromine_get_query("fbi"))
  })
  resp2 <- fixture$get({
    sy(async_acromine_get_query("notanacronym"))
  })
  resp3 <- fixture$get({
    sy(async_acromine_get_query("cli"))
  })

  ans <- acromine_get_process("fbi", resp)
  ans2 <- acromine_get_process("fbi", resp2)
  ans3 <- acromine_get_process("fbi", resp3)
  expect_snapshot({
    writeLines(format(ans))
    writeLines(format(ans2))
    writeLines(format(ans3))
  })
})

test_that("async_profanity_get", {
  resp <- fixture$get({
    sy(async_profanity_get_query("nope"))
  })
  mockery::stub(
    async_profanity_get,
    "async_profanity_get_query",
    function(...) async_constant(resp)
  )
  ans <- sy(async_profanity_get("nope"))
  expect_s3_class(ans, "pkg_name_check_profanity")
  expect_false(ans)
})

test_that("async_profanity_get_query", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(PKG_NAME_CHECK_PROFANITY_URL = check_app$url("/echo"))
  ret <- synchronize(async_profanity_get_query("foobar"))
  expect_snapshot(show_request(ret))
})

test_that("profanity_get_process", {
  resp <- fixture$get({
    sy(async_profanity_get_query("nope"))
  })
  ans <- profanity_get_process("nope", resp)
  expect_s3_class(ans, "pkg_name_check_profanity")
  expect_false(ans)
})

sentiment_ex <- function() c(abuse = -3, irony = -1, xo = 3, xoxoxo = 4)

test_that("async_sentiment_get", {
  expect_true(is.logical(sentiment_get_has_data()))

  mockery::stub(async_sentiment_get, "sentiment_get_has_data", FALSE)
  mockery::stub(
    async_sentiment_get,
    "async_sentiment_get_data",
    function(...) async_constant(sentiment_ex())
  )

  ans <- sy(async_sentiment_get("xo"))
  expect_equal(
    ans,
    structure(c(xo = 3), class = c("pkg_name_check_sentiment", "numeric"))
  )

  mockery::stub(async_sentiment_get, "sentiment_get_has_data", TRUE)
  mockery::stub(
    async_sentiment_get,
    "async_constant",
    function(...) async_constant(sentiment_ex())
  )

  ans <- sy(async_sentiment_get("xo"))
})

test_that("async_sentiment_get_data", {
  withr::local_envvar(
    PKG_NAME_CHECK_SENTIMENT_URL = check_app$url("/sentiment")
  )
  ans <- sy(async_sentiment_get_data())
  pkgd_data$sentiment <- NULL
  expect_equal(ans, sentiment_ex())
})

test_that("sentiment_string", {
  local_reproducible_output()
  expect_true(is_ascii(sentiment_string(1)))
  mockery::stub(sentiment_string, "has_emoji", TRUE)
  expect_false(is_ascii(sentiment_string(1)))
})

test_that("format.pkg_name_check_sentiment", {
  local_edition(3)
  local_reproducible_output()
  mockery::stub(async_sentiment_get, "sentiment_get_has_data", FALSE)
  withr::local_envvar(
    PKG_NAME_CHECK_SENTIMENT_URL = check_app$url("/sentiment")
  )
  ans1 <- sy(async_sentiment_get("xo"))
  ans2 <- sy(async_sentiment_get("something"))
  pkgd_data$sentiment <- NULL
  expect_snapshot({
    writeLines(format(ans1))
    writeLines(format(ans2))
  })
})

test_that("async_urban_get", {
  local_edition(3)
  local_reproducible_output()
  mockery::stub(async_urban_get, "async_urban_get_query", function(...) {
    async_constant(fixture$get({
      sy(async_urban_get_query("tool"))
    }))
  })
  ans <- sy(async_urban_get("tool"))
  expect_s3_class(ans, "pkg_name_check_urban")
  expect_s3_class(ans, "tbl_df")
  expect_snapshot(ans)
})

test_that("async_urban_get_query", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(PKG_NAME_CHECK_URBAN_URL = check_app$url("/echo/"))
  ret <- synchronize(async_urban_get_query("foobar"))
  expect_snapshot(show_request(ret))
})

test_that("urban_get_process", {
  local_edition(3)
  local_reproducible_output()
  resp <- fixture$get({
    sy(async_urban_get_query("tool"))
  })
  ans <- urban_get_process("tool", resp)
  expect_s3_class(ans, "pkg_name_check_urban")
  expect_s3_class(ans, "tbl_df")
  expect_snapshot(ans)
})

test_that("format.pkg_name_check_urban", {
  local_edition(3)
  local_reproducible_output()
  ans <- fixture$get({
    sy(async_urban_get("tool"))
  })
  ans2 <- fixture$get({
    sy(async_urban_get("not-this-one-asdfsf"))
  })
  expect_snapshot(writeLines(format(ans)))
  expect_snapshot(writeLines(format(ans2)))

  ans <- ans[1,]
  ans$definition[1] <- paste(rep(ans$definition[1], 10), collapse = "\n")
  expect_snapshot(writeLines(format(ans)))
})

test_that("clean_urban", {
  expect_equal(clean_urban("foo \r\n\r\n bar"), "foo      bar")
  expect_equal(clean_urban("foo \r\n\n bar"), "foo     bar")
  expect_equal(clean_urban("foo \n\n bar"), "foo    bar")
})

test_that("async_pnc_bioc", {
   expect_equal(sy(async_pnc_bioc("bim")), sy(pnc_bioc_false("bim")))
   expect_equal(sy(async_pnc_bioc("yeast2")), sy(pnc_bioc_false("yeast2")))

   mockery::stub(
     async_pnc_bioc,
     "async_pnc_bioc_web",
     function(...) async_constant(pnc_bioc_false("Biobase"))
   )
   expect_equal(sy(async_pnc_bioc("Biobase")), sy(pnc_bioc_false("Biobase")))
})

# "pnc_bioc_false"

test_that("async_pnc_bioc_web", {
  local_edition(3)
  local_reproducible_output()
  response <- fixture$get({
    sy(async_pnc_bioc_query("all"))
  })
  mockery::stub(
    async_pnc_bioc_web,
    "async_pnc_bioc_query",
    function(...) async_constant(response)
  )
  ans <- sy(async_pnc_bioc_web("all"))
  expect_snapshot(ans)
})

test_that("pnc_bioc_query", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(
    PKG_NAME_CHECK_BIOC_URL = check_app$url("/bioc/")
  )
  mockery::stub(
    async_pnc_bioc_query,
    "pkgcache::bioc_repos",
    list(BioCann = check_app$url("/biocann/"))
  )
  ans <- pnc_bioc_process("all", sy(async_pnc_bioc_query("all")))
  ans2 <- pnc_bioc_process("Agcdf", sy(async_pnc_bioc_query("Agcdf")))
  ans3 <- pnc_bioc_process("axx", sy(async_pnc_bioc_query("axx")))
  expect_snapshot(ans)
  expect_snapshot(ans2)
  expect_snapshot(ans3)
})

test_that("pnc_bioc_process", {
  response <- fixture$get({
    sy(async_pnc_bioc_query("all"))
  })
  ans <- pnc_bioc_process("all", response)
  expect_s3_class(ans, "pkg_name_check_bioc")
  expect_false(ans$bioc)
  expect_equal(ans$package, "ALL")
})

test_that("pnc_bioc_parse", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(
    PKG_NAME_CHECK_BIOC_URL = check_app$url("/bioc/"),
    PKG_NAME_CHECK_BIOC_ANN_URL = check_app$url("/biocann/src/contrib/PACKAGES.gz")
  )
  pkg1 <- pnc_bioc_parse(sy(async_pnc_bioc_query("all"))[[1]])
  pkg2 <- pnc_bioc_parse(sy(async_pnc_bioc_query("all"))[[2]])
  expect_snapshot(pkg1)
  expect_snapshot(pkg2)
})

test_that("pnc_bioc_parse_pgz", {
  local_edition(3)
  local_reproducible_output()
  withr::local_envvar(
    PKG_NAME_CHECK_BIOC_URL = check_app$url("/bioc/"),
    PKG_NAME_CHECK_BIOC_ANN_URL = check_app$url("/biocann/src/contrib/PACKAGES.gz")
  )
  pkg <- pnc_bioc_parse_pgz(sy(async_pnc_bioc_query("all"))[[3]])
  expect_snapshot(pkg)
})

test_that("pnc_bioc_removed", {
  expect_true(is.character(pnc_bioc_removed()))
})

test_that("pnc_bioc_old_annotation", {
  expect_true(is.character(pnc_bioc_old_annotation()))
})
