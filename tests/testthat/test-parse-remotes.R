
context("parse_remotes")

test_that("parse_remotes, standard", {

  cases <- list(
    list("pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2")),
    list("pkg@>=2.9",
         list(package = "pkg", atleast = ">=", version = "2.9")),
    list("pkg@last",
         list(package = "pkg", atleast = "", version = "last")),
    list("standard::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("standard::pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2"))
  )

  expect_equal(
    get_remote_types(vcapply(cases, "[[", 1)),
    rep("standard", length(cases))
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_remotes(case[[1]])[[1]],
      c(case[[2]], ref = case[[1]], type = "standard")
    )
    expect_s3_class(p, c("remote_ref_cran", "remote_ref"))
  }

})

test_that("parse_remotes, cran", {

  cases <- list(
    list("cran::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("cran::pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2"))
  )

  expect_equal(
    get_remote_types(vcapply(cases, "[[", 1)),
    rep("cran", length(cases))
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_remotes(case[[1]])[[1]],
      c(case[[2]], ref = case[[1]], type = "cran")
    )
    expect_s3_class(p, c("remote_ref_cran", "remote_ref"))
  }

})

test_that("parse_remotes, github", {

  cases <- list(
    list("user/repo"),
    list("github::user/repo"),
    list("pkg=user/repo", package = "pkg"),
    list("pkg=github::user/repo", package = "pkg"),
    list("user/repo/subdir", subdir = "subdir"),
    list("user/repo@badcafe", commitish = "badcafe"),
    list("user/repo#123", pull = "123"),
    list("user/repo@*release", release = "*release"),
    list("github::user/repo/subdir", subdir = "subdir"),
    list("github::user/repo@badcafe", commitish = "badcafe"),
    list("github::user/repo#123", pull = "123"),
    list("github::user/repo@*release", release = "*release"),
    list("pkg=user/repo/subdir", package = "pkg", subdir = "subdir"),
    list("pkg=user/repo@badcafe", package = "pkg", commitish = "badcafe"),
    list("pkg=user/repo#123", package = "pkg", pull = "123"),
    list("pkg=user/repo@*release", package = "pkg", release = "*release"),

    # github url cases
    list("git@github.com:user/repo.git"),
    list("git@github.ubc.ca:user/repo.git"),
    list("https://github.com/user/repo"),
    list("https://github.ubc.ca/user/repo"),
    list("https://github.com/user/repo/tree/i-am-a-branch", commitish = "i-am-a-branch"),
    list("https://github.com/user/repo/commit/1234567", commitish = "1234567"),
    list("https://github.com/user/repo/pull/108", pull = "108"),
    list("https://github.com/user/repo/releases/tag/1.0.0", commitish = "1.0.0"),
    list("https://github.com/user/repo/releases/latest", release = "latest"),
    list("https://github.com/user/repo/releases/latest", release = "latest"),
    list("https://github.com/foo/bar", username = "foo", repo = "bar"),
    list("git@github.com:foo/bar.git", username = "foo", repo = "bar"),

    # Username and repo can have hyphens in them
    list("git@github.com:foo-bar/baz-qux.git", username = "foo-bar", repo = "baz-qux")
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_remotes(case[[1]])[[1]],
      utils::modifyList(
        list(package = case$repo %||% "repo", username = "user",
             repo = "repo", subdir = "", commitish = "", pull = "",
             release = "", ref = case[[1]], type = "github"),
        case[-1]
      )
    )
    expect_s3_class(p, c("remote_ref_github", "remote_ref"))
  }
})

test_that("parse_remotes error on unknown type", {
  expect_error(parse_remotes("my_package"), "parse remotes")
})
