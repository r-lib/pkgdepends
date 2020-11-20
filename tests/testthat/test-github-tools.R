
test_that("type_github_get_data, sha, description", {
  skip_if_offline()

  is_sha <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x) &&
      grepl("^[0-9a-f]+$", x)
  }

  cases <- c(
    "r-lib/pak",
    "r-lib/pak@HEAD",
    "r-lib/pak@v0.1.2",
    "r-lib/pak@e65de1e9630d",
    "r-lib/pak@e65de1e9630dbfcaf1044718b742bf806486b107",
    "r-lib/pak#90",
    "wesm/feather/R@ec40c1eae1ac83b86fc41bb2f5cd916152d19015"
  )

  synchronise(async_map(cases, function(c) {
    rem <- parse_pkg_ref(c)
    type_github_get_data(rem)$
      then(function(data) {
        expect_true(is_sha(data$sha), info = c)
        expect_true(inherits(data$description, "description"))
      })
  }))
})

test_that("type_github_get_data, no such user", {
  skip_if_offline()

  rem <- parse_pkg_ref("r-lib-xxx-xxx/pak")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find GitHub repo .*r-lib-xxx-xxx.*",
    class = "github_error"
  )

  rem <- parse_pkg_ref("r-lib-xxx-xxx/pak#90")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find GitHub repo .*r-lib-xxx-xxx.*",
    class = "github_error"
  )
})

test_that("type_github_get_data, no such repo", {
  skip_if_offline()

  rem <- parse_pkg_ref("r-lib/pak-xxx-xxx")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "GitHub repo .*pak-xxx-xxx.*",
    class = "github_error"
  )

  rem <- parse_pkg_ref("r-lib/pak-xxx-xxx#90")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "GitHub repo .*pak-xxx-xxx.*",
    class = "github_error"
  )
})

test_that("github_query, invalid PAT", {
  skip_if_offline()
  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = "invalid",
    GITHUB_TOKEN = "invalid",
    GITHUB_PAT = "invalid",
    CI_GITHUB_TOKEN = "invalid"
  ))

  rem <- parse_pkg_ref("r-lib/pak-xxx-xxx")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Bad GitHub credentials, make sure that your GitHub token is valid",
    class = "github_error"
  )

  rem <- parse_pkg_ref("r-lib/pak-xxx-xxx#90")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Bad GitHub credentials, make sure that your GitHub token is valid",
    class = "github_error"
  )
})

test_that("github_query, no internet", {
  # TODO: we can test this with toxiproxy
  expect_true(TRUE)
})

test_that("github_query, rate limited", {
  rem <- parse_pkg_ref("r-lib/pak")
  res <- readRDS(test_path("fixtures/gh-rate-limit-response.rds"))
  json <- rawToChar(res$content)
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_error(
    throw(new_github_query_error(rem, res, obj, NULL)),
    "API rate limit exceeded. Rate limit will reset at",
    class = "github_error"
  )
})

test_that("github_query, access denied", {
  skip_if_offline()
  withr::local_envvar(
    c(GITHUB_PAT_GITHUB_COM = NA_character_,
      GITHUB_TOKEN = NA_character_,
      GITHUB_PAT = NA_character_)
  )

  rem <- parse_pkg_ref("gaborcsardi/secret-test")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "GitHub repo .*secret-test*",
    class = "github_error"
  )
})

test_that("cannot find R package on GitHub, no DESCRIPTION", {
  skip_if_offline()

  rem <- parse_pkg_ref("tidyverse/tidyverse.org")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find R package in GitHub repo tidyverse/tidyverse.org",
    class = "github_error"
  )

  rem <- parse_pkg_ref("r-lib/crayon/R")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find R package in GitHub repo r-lib/crayon in directory 'R'",
    class = "github_error"
  )

  rem <- parse_pkg_ref("r-lib/crayon/R#79")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find R package in GitHub repo r-lib/crayon in directory 'R'",
    class = "github_error"
  )
})

test_that("cannot parse DESCRIPTION on GH", {
  skip_if_offline()

  ref <- "r-lib/pkgdepends/tests/testthat/fixtures/bad-desc@f5a84c34f5"
  expect_error(
    synchronise(type_github_get_data(parse_pkg_ref(ref))),
    "Can't parse DESCRIPTION file in GitHub repo",
    class = "github_error"
  )

  ref <- "r-lib/pkgdepends/tests/testthat/fixtures/bad-desc#144"
  expect_error(
    synchronise(type_github_get_data(parse_pkg_ref(ref))),
    "Can't parse DESCRIPTION file in GitHub repo",
    class = "github_error"
  )
})

test_that("http error", {
  # Do not send a real token to httpbin.org
  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = "foobar",
    GITHUB_TOKEN = "foobar",
    GITHUB_PAT = "foobar"
  ))

  err <- tryCatch(
    synchronise(github_query("foobar", url = "https://httpbin.org/status/404")),
    error = function(e) e
  )

  expect_match(err$message, "GitHub HTTP error", fixed = TRUE)
  expect_match(err$parent$message, "Not Found (HTTP 404)", fixed = TRUE)
})

test_that("no such PR error", {
  rem <- parse_pkg_ref("r-lib/pak#89")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find PR #89 in",
    class = "github_error"
  )
})

test_that("no such ref error", {
  rem <- parse_pkg_ref("r-lib/pak@bad-ref-no-no-no")
  expect_error(
    synchronise(type_github_get_data(rem)),
    "Can't find reference @bad-ref-no-no-no in GitHub repo r-lib/pak",
    fixed = TRUE,
    class = "github_error"
  )
})
