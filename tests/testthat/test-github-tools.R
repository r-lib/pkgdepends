test_that("type_github_get_data, sha, description", {
  setup_fake_gh_app()

  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak")))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@HEAD")))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@v0.1.2")))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@*release")))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref(
      "r-lib/pak@e65de1e9630d"
    )))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref(
      "r-lib/pak@e65de1e9630dbfcaf1044718b742bf806486b107"
    )))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak#90")))
  )
  expect_snapshot(
    synchronise(type_github_get_data(parse_pkg_ref(
      "wesm/feather/R@ec40c1eae1ac83b86fc41bb2f5cd916152d19015"
    )))
  )
})

test_that("type_github_get_data, no such user", {
  setup_fake_gh_app()
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib-xxx-xxx/pak")))
  )
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib-xxx-xxx/pak#90")))
  )
})

test_that("type_github_get_data, no such repo", {
  setup_fake_gh_app()
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx")))
  )
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx#90")))
  )
})

test_that("github_query, invalid PAT", {
  setup_fake_gh_app()

  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = "invalid",
    GITHUB_TOKEN = "invalid",
    GITHUB_PAT = "invalid",
    CI_GITHUB_TOKEN = "invalid"
  ))

  expect_snapshot(
    error = TRUE,
    transform = transform_no_srcref,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx")))
  )
  expect_snapshot(
    error = TRUE,
    transform = transform_no_srcref,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx#90")))
  )
})

test_that("github_query, no internet", {
  withr::local_envvar(R_PKG_GITHUB_API_URL = "https://127.0.0.1:80")
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak")))
  )
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
  setup_fake_gh_app()
  withr::local_envvar(
    c(
      GITHUB_PAT_GITHUB_COM = NA_character_,
      GITHUB_TOKEN = NA_character_,
      GITHUB_PAT = NA_character_
    )
  )

  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("gaborcsardi/secret-test")))
  )
})

test_that("cannot find R package on GitHub, no DESCRIPTION", {
  setup_fake_gh_app()

  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("tidyverse/tidyverse.org")))
  )

  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/cranyon/R")))
  )

  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/crayon/R#79")))
  )
})

test_that("cannot parse DESCRIPTION on GH", {
  setup_fake_gh_app()

  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/bad@main"))),
    transform = transform_no_srcref
  )

  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/bad#100"))),
    transform = transform_no_srcref
  )

  # binary description
  expect_snapshot(
    error = TRUE,
    synchronize(type_github_get_data(parse_pkg_ref("r-lib/bad/bin@main"))),
    transform = transform_no_srcref
  )
})

test_that("http error", {
  withr::local_envvar(
    R_PKG_GITHUB_API_URL = paste0(fake_gh$url(), "/404")
  )

  expect_snapshot(
    error = TRUE,
    transform = transform_no_srcref,
    synchronise(type_github_get_data(parse_pkg_ref("foo/bar")))
  )
})

test_that("no such PR error", {
  setup_fake_gh_app()
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak#89"))),
    transform = transform_no_srcref
  )
})

test_that("no such ref error", {
  setup_fake_gh_app()
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref(
      "r-lib/pak@bad-ref-no-no-no"
    ))),
    transform = transform_no_srcref
  )
})

test_that("no release error", {
  setup_fake_gh_app()
  expect_snapshot(
    error = TRUE,
    synchronise(type_github_get_data(parse_pkg_ref("r-lib/bad@*release"))),
    transform = transform_no_srcref
  )
})

test_that("builtin token messages once per session", {
  once_per_session(reset = TRUE)
  mockery::stub(type_github_builtin_token, "sample", "builtin-token")
  expect_snapshot(type_github_builtin_token())
  expect_snapshot(type_github_builtin_token())
})

test_that("CI specific token is picked up if set", {
  withr::local_envvar(
    "CI_GITHUB_TOKEN" = "ci-token",
    "CI" = "true"
  )
  expect_snapshot(type_github_get_headers())
})

test_that("builtin token is used if no other token is available", {
  withr::local_envvar(
    "CI" = NA_character_,
    GITHUB_PAT = NA_character_,
    GITHUB_TOKEN = NA_character_,
    GITHUB_PAT_GITHUB_COM = "FAIL"
  )
  mockery::stub(
    type_github_get_headers,
    "type_github_builtin_token",
    "builtin-token"
  )
  expect_snapshot(type_github_get_headers())
})
