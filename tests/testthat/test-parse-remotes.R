
test_that("package_name_rx", {
  good <- c("A1", "a1", "Z1", "z1", "foo.bar", "foo.bar.baz", "a1.b2")

  for (t in good) expect_true(is_valid_package_name(t), info = t)

  bad <- list(
    c("pkg", "forbidden"),
    c("pak\u00e1ge", "ASCII"),
    c("good-package", "letters, numbers and dot"),
    c("x", "two characters"),
    c("1stpackage", "must start with a letter"),
    c("dots.", "must not end with a dot")
  )

  for (t in bad) {
    ans <- is_valid_package_name(t[1])
    expect_false(ans, info = t[1])
    expect_match(attr(ans, "reason"), t[2], info = t[1])
  }
})

test_that("parse_pkg_refs, standard", {

  cases <- list(
    list("pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("pkg@0.1-2",
         list(package = "pkg", atleast = "==", version = "0.1-2")),
    list("pkg@>=2.9",
         list(package = "pkg", atleast = ">=", version = "2.9")),
    list("pkg@last",
         list(package = "pkg", atleast = "==", version = "last")),
    list("standard::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("standard::pkg@0.1-2",
         list(package = "pkg", atleast = "==", version = "0.1-2"))
  )

  expect_equal(
    get_remote_types(vcapply(cases, "[[", 1)),
    rep("standard", length(cases))
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_pkg_refs(case[[1]])[[1]],
      c(case[[2]], list(ref = case[[1]], type = "standard", params = character()))
    )
    expect_s3_class(p, c("remote_ref_cran", "remote_ref"))
  }

})

test_that("parse_pkg_refs, cran", {

  cases <- list(
    list("cran::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("cran::pkg@0.1-2",
         list(package = "pkg", atleast = "==", version = "0.1-2"))
  )

  expect_equal(
    get_remote_types(vcapply(cases, "[[", 1)),
    rep("cran", length(cases))
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_pkg_refs(case[[1]])[[1]],
      c(case[[2]], list(ref = case[[1]], type = "cran", params = character()))
    )
    expect_s3_class(p, c("remote_ref_cran", "remote_ref"))
  }

})

test_that("github regexes", {
  username <- list(
    list("foobar", "foobar"),
    list("", NA_character_),
    list("-bad", NA_character_),
    list("123456789012345678901234567890123456789",
         "123456789012345678901234567890123456789"),
    list("1234567890123456789012345678901234567890", NA_character_)
  )
  for (c in username) {
    rx <- paste0("^", github_username_rx(), "$")
    expect_equal(
      re_match(c[[1]], rx)$username,
      c[[2]]
    )
  }

  commitish <- list(
    list("", NA_character_),
    list("x", NA_character_),
    list("foobar", NA_character_),
    list("@foobar", "foobar"),
    list("@*foobar", NA_character_)
  )
  for (c in commitish) {
    expect_equal(
      re_match(c[[1]], github_commitish_rx())$commitish,
      c[[2]]
    )
  }

  pull <- list(
    list("", NA_character_),
    list("x", NA_character_),
    list("@foobar", NA_character_),
    list("#", NA_character_),
    list("#123", "123"),
    list("#1", "1"),
    list("#foobar", NA_character_),
    list("#1foo", "1")
  )
  for (c in pull) {
    expect_equal(
      re_match(c[[1]], github_pull_rx())$pull,
      c[[2]]
    )
  }

  release <- list(
    list("", NA_character_),
    list("x", NA_character_),
    list("@foobar", NA_character_),
    list("@*foobar", NA_character_),
    list("@*release", "*release")
  )
  for (c in release) {
    expect_equal(
      re_match(c[[1]], github_release_rx())$release,
      c[[2]]
    )
  }

  detail <- list(
    list("@foobar",   c("foobar", "",    "")),
    list("#123",       c("",       "123", "")),
    list("@*release", c("",       "",    "*release")),
    list("foobar",     c("",       "",    ""))
  )
  for (c in detail) {
    expect_equal(
      unlist(re_match(
        c[[1]],
        github_detail_rx())[, c("commitish", "pull", "release")]),
      structure(c[[2]], names = c("commitish", "pull", "release"))
    )
  }
})

test_that("parse_pkg_refs error on unknown type", {
  expect_snapshot(
    error = TRUE,
    parse_pkg_refs(c("notgood::pkg", "good", "my_package"))
  )
})

test_that("custom remote types", {
  xspecs <- NULL
  xargs <- NULL
  parse_remote_foo <- function(specs, ...) {
    xspecs <<- specs
    xargs <<- list(...)
    list(list())
  }
  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(parse = parse_remote_foo))),
    parse_pkg_refs("foo::arbitrary_string/xxx", ex1 = "1", ex2 = "2")
  )
  expect_identical(
    res,
    list(structure(list(type = "foo", params = character()),
                   class = c("remote_ref_foo", "remote_ref", "list")))
  )
  expect_identical(xspecs, "foo::arbitrary_string/xxx")
  expect_identical(xargs, list(ex1 = "1", ex2 = "2"))

  res2 <- parse_pkg_refs(
    "foo::arbitrary_string/xxx", ex1 = "1", ex2 = "2",
    remote_types = list(foo = list(parse = parse_remote_foo)))
  expect_identical(res, res2)
})

test_that("type_default_parse", {
  res <- type_default_parse(c("foo::bar", "package=foo2::bar2"))
  expect_identical(res,
    list(
      list(package = "", type = "foo", rest = "bar", ref = "foo::bar"),
      list(package = "package", type = "foo2", rest = "bar2",
           ref = "package=foo2::bar2")
    )
  )
})

test_that("default parse function", {
  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(), foo2 = list())),
    parse_pkg_refs(c("foo::bar", "package=foo2::bar2"))
  )
  expect_identical(res,
    list(
      structure(list(package = "", type = "foo", rest = "bar", ref = "foo::bar",
                     params = character()),
                class = c("remote_ref_foo", "remote_ref", "list")),
      structure(list(package = "package", type = "foo2", rest = "bar2",
                     ref = "package=foo2::bar2", params = character()),
                class = c("remote_ref_foo2", "remote_ref", "list"))
    )
  )

  res2 <- parse_pkg_refs(
    c("foo::bar", "package=foo2::bar2"),
    remote_types = list(foo = list(), foo2 = list()))
  expect_identical(res, res2)
})

test_that("parse_pkg_refs, local", {

  cases <- list(
    list("local::path", "path"),
    list("local::/path", "/path"),
    list("local::~/path", "~/path"),
    list("local::./path", "./path"),
    list("local::\\path", "\\path"),
    list("/path", "/path"),
    list("~/path", "~/path"),
    list("./path", "./path"),
    list(".\\path", ".\\path"),
    list("\\path", "\\path"),
    list(".", ".")
  )

  for (c in cases) {
    expect_equal(
      parse_pkg_ref(c[[1]]),
      structure(
        list(package = NA_character_, path = c[[2]],
             ref = paste0("local::", c[[2]]), type = "local",
             params = character()),
        class = c("remote_ref_local", "remote_ref", "list")
      )
    )
  }
})

test_that("parse_query", {
  empty <- c(a = "1")[-1]
  cases <- list(
    list("", empty),
    list("?", empty),
    list("?foo", c(foo = "")),
    list("?foo=1", c(foo = "1")),
    list("?foo&bar", c(foo = "", bar = "")),
    list("?foo=1&bar=2&foo", c(foo = "1", bar = "2", foo = "")),
    list("?foo=1%202&bar=x", c(foo = "1 2", bar = "x"))
  )

  for (c in cases) {
    suppressMessages(expect_equal(parse_query(c[[1]]), c[[2]]))
  }
})

test_that("parameters", {
  cases <- list(
    list("foo", "?bar", c(bar = "")),
    list("foo", "?bar=1&foo&bar=11", c(bar = "1", foo = "", bar = "11")),
    list("user/repo", "?source", c(source = ""))
  )

  for (c in cases) {
    wo <- parse_pkg_ref(c[[1]])
    suppressMessages(wi <- parse_pkg_ref(paste0(c[[1]], c[[2]])))
    wo$params <- c[[3]]
    expect_equal(wi, wo)
  }
})

test_that("explicit package names", {
  expect_snapshot({
    parse_pkg_ref("package=user/notpackage")
    parse_pkg_ref("package=user/notpackage@tag")
    parse_pkg_ref("package=github::user/notpackage@tag")

    parse_pkg_ref("package=local::/abs/path")
    parse_pkg_ref("package=local::rel/path")
    parse_pkg_ref("package=local::~/home/path")
    parse_pkg_ref("package=/abs/path")
    parse_pkg_ref("package=./rel/path")
    parse_pkg_ref("package=~/home/path")

    parse_pkg_ref("package=url::https://example.com/p1.tar.gz")
  })
})
