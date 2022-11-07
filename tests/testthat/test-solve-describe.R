
test_that("conflicting dependencies", {
  pkgs <- make_fake_resolution(
    `aa` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cran::cc"))),
    `bb` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cc/cc"))),
    `cran::cc` = list(),
    `cc/cc` = list(extra = list(list(remotesha = "badcafe")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("aa", "cran::cc"))
  expect_equal(dsc$type, c("standard", "cran"))
  expect_equal(dsc$direct, c(TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK"))
  expect_equal(dsc$package, c("aa", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "conflict"))
  expect_snapshot(dsc)
})

test_that("conflicting dependencies downstream", {
  pkgs <- make_fake_resolution(
    `a0` = list(direct = TRUE, deps = list(make_fake_deps(Imports = "aa, bb"))),
    `aa` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cran::cc"))),
    `bb` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cc/cc"))),
    `cran::cc` = list(),
    `cc/cc` = list(extra = list(list(sha = "badcafe")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("a0", "bb", "cc/cc"))
  expect_equal(dsc$type, c("standard", "standard", "github"))
  expect_equal(dsc$direct, c(TRUE, TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK", "OK"))
  expect_equal(dsc$package, c("a0", "bb", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "dep-failed", "conflict"))
  expect_snapshot(dsc)
})
