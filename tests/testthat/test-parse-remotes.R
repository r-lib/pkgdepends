
context("parse_remotes")

test_that("parse_remotes, cran", {

  cases <- list(
    list("pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2")),
    list("pkg@>=2.9",
         list(package = "pkg", atleast = ">=", version = "2.9")),
    list("pkg@last",
         list(package = "pkg", atleast = "", version = "last")),
    list("cran::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("cran::pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2"))
  )

  for (case in cases) {
    expect_equal_named_lists(
      parse_remotes(case[[1]])[[1]],
      c(case[[2]], ref = case[[1]], type = "cran")
    )
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
    list("pkg=user/repo@*release", package = "pkg", release = "*release")
  )

  for (case in cases) {
    expect_equal_named_lists(
      parse_remotes(case[[1]])[[1]],
      utils::modifyList(
        list(package = case$repo %||% "repo", username = "user",
             repo = "repo", subdir = "", commitish = "", pull = "",
             release = "", ref = case[[1]], type = "github"),
        case[-1]
      )
    )
  }
})
