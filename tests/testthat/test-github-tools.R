
context("github utility functions")

make_gh_ref_query <- function(user, repo, ref) {
  glue(
    "query {
       repository(owner: \"<user>\", name: \"<repo>\") {
         object(expression: \"<ref>\") {
           oid
         }
       }
    }",
    .open = "<", .close = ">"
  )
}

test_that("github_query", {
  skip_if_offline()

  q <- make_gh_ref_query("r-lib", "pak", "master")
  selector <- c("data", "repository", "object", "oid")

  ret <- synchronise(github_query(q, selector))
  expect_true(is.character(ret))
  expect_true(length(ret) == 1)
})

test_that("github_query, no such user", {
  skip_if_offline()

  q <- make_gh_ref_query("r-lib-xxx-xxx", "pak", "master")
  selector <- c("data", "repository", "object", "oid")

  expect_error(
    synchronise(github_query(q, selector)),
    "username .*r-lib-xxx-xxx.*",
    class = "github_error"
  )
})

test_that("github_query, no such repo", {
  skip_if_offline()

  q <- make_gh_ref_query("r-lib", "pak-xxx-xxx", "master")
  selector <- c("data", "repository", "object", "oid")

  expect_error(
    synchronise(github_query(q, selector)),
    "Repository with the name .*pak-xxx-xxx.*",
    class = "github_error"
  )
})

test_that("github_query, invalid PAT", {
  skip_if_offline()
  withr::local_envvar(c("GITHUB_TOKEN" = "invalid"))

  q <- make_gh_ref_query("r-lib", "pak", "master")
  selector <- c("data", "repository", "object", "oid")

  expect_error(
    synchronise(github_query(q, selector)),
    "Bad GitHub credentials, make sure that your GitHub token is valid",
    class = "github_error"
  )
})

test_that("github_query, no internet", {
  # TODO: we can test this with toxiproxy
  expect_true(TRUE)
})

test_that("github_query, rate limited", {
  res <- readRDS(test_path("fixtures/gh-rate-limit-response.rds"))
  json <- rawToChar(res$content)
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_error(
    throw(new_github_query_error(obj, res, NULL)),
    "API rate limit exceeded. Rate limit will reset at",
    class = "github_error"
  )
})

test_that("github_query, access denied", {
  skip_if_offline()
  q <- make_gh_ref_query("gaborcsardi", "secret-test", "master")
  selector <- c("data", "repository")

  expect_error(
    synchronise(github_query(q, selector)),
    "Repository with the name .*secret-test*",
    class = "github_error"
  )
})

test_that("type_github_get_commit_sha", {
  skip_if_offline()

  is_sha <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x) &&
      grepl("^[0-9a-f]+$", x)
  }

  cases <- c(
    "r-lib/pak",
    "r-lib/pak@master",
    "r-lib/pak@v0.1.2",
    "r-lib/pak@e65de1e9630d",
    "r-lib/pak@e65de1e9630dbfcaf1044718b742bf806486b107",
    "r-lib/pak#90"
  )

  synchronise(async_map(cases, function(c) {
    rem <- parse_pkg_ref(c)
    type_github_get_commit_sha(rem)$
      then(function(sha) expect_true(is_sha(sha), info = c))
  }))
})

test_that("type_github_get_commit_sha errors", {
  skip_if_offline()

  cases <- list(
    c("r-lib-xxxxxx/pak", "Could not resolve to a User with the username"),
    c("r-lib/pak-xxxxxx", "Could not resolve to a Repository with the name")
  )

  synchronise(async_map(cases, function(c) {
    rem <- parse_pkg_ref(c[[1]])
    type_github_get_description_data(rem)$
      catch(error = function(e) { expect_match(e$message, c[[2]]) })
  }))
})

test_that("type_github_get_description_data", {
  skip_if_offline()

  cases <- c(
    "r-lib/pak",
    "r-lib/pak@master",
    "r-lib/pak@v0.1.2",
    "r-lib/pak@e65de1e9630d",
    "r-lib/pak@e65de1e9630dbfcaf1044718b742bf806486b107",
    "r-lib/pak#90",
    "wesm/feather/R@ec40c1eae1ac83b86fc41bb2f5cd916152d19015"
  )

  synchronise(async_map(cases, function(c) {
    rem <- parse_pkg_ref(c)
    type_github_get_description_data(rem)$
      then(function(dsc) expect_true(inherits(dsc, "description")))
  }))
})

test_that("cannot find R package on GitHub, no DESCRIPTION", {
  skip_if_offline()

  err <- tryCatch(
    synchronise(
      type_github_get_description_data(parse_pkg_ref("r-lib/crayon/R"))
    ),
    error = function(e) e
  )
  expect_equal(
    err$message,
    "Cannot find R package in GitHub repo `r-lib/crayon`, in directory `R`"
  )
})

test_that("cannot parse DESCRIPTION on GH", {
  skip_if_offline()

  ref <- "r-lib/pkgdepends/tests/testthat/fixtures/bad-desc@f5a84c34f5"
  err <- tryCatch(
    synchronise(
      type_github_get_description_data(parse_pkg_ref(ref))
    ),
    error = function(e) e
  )
  expect_match(
    err$message,
    "Cannot parse DESCRIPTION file in GitHub repo"
  )
})
