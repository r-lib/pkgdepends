test_that("satisfies_remote", {
  res <- make_fake_resolution(`cran::crayon@>=1.0.0` = list())

  ## GitHub type is never good
  bad1 <- make_fake_resolution(`github::r-lib/crayon` = list())
  expect_false(ans <- satisfy_remote_cran(res, bad1))
  expect_match(attr(ans, "reason"), "Type must be")

  ## Missing DESCRIPTION for installed type
  bad2 <- make_fake_resolution(`installed::foobar` = list())
  expect_false(ans <- satisfy_remote_cran(res, bad2))
  expect_match(attr(ans, "reason"), "not from CRAN")

  ## installed, but not from CRAN
  fake_desc <- desc::desc("!new")
  fake_desc$set(Repository = "Not CRAN")
  bad3 <- make_fake_resolution(
    `installed::foobar` = list(
      extra = list(list(description = fake_desc))
    )
  )
  expect_false(ans <- satisfy_remote_cran(res, bad3))
  expect_match(attr(ans, "reason"), "not from CRAN")

  ## CRAN type, but package name does not match
  bad4 <- make_fake_resolution(`cran::crayon2` = list())
  expect_false(ans <- satisfy_remote_cran(res, bad4))
  expect_match(attr(ans, "reason"), "names differ")

  ## installed type, but package name does not match
  bad5 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "crayon2",
      extra = list(list(repotype = "cran"))
    )
  )
  expect_false(ans <- satisfy_remote_cran(res, bad5))
  expect_match(attr(ans, "reason"), "names differ")

  ## CRAN type, but version is not good enough
  bad6 <- make_fake_resolution(`cran::crayon` = list(version = "0.0.1"))
  expect_false(ans <- satisfy_remote_cran(res, bad6))
  expect_match(attr(ans, "reason"), "Insufficient version")

  ## Same version, CRAN
  ok1 <- make_fake_resolution(`cran::crayon` = list())
  expect_true(satisfy_remote_cran(res, ok1))

  ## Newer version, CRAN
  ok2 <- make_fake_resolution(`cran::crayon` = list(version = "2.0.0"))
  expect_true(satisfy_remote_cran(res, ok2))

  ## Same version, installed
  ok3 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "crayon",
      extra = list(list(repotype = "cran"))
    )
  )
  expect_true(satisfy_remote_cran(res, ok3))

  ## Newer version, installed
  ok4 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "crayon",
      version = "2.0.0",
      extra = list(list(repotype = "cran"))
    )
  )
  expect_true(satisfy_remote_cran(res, ok4))

  # direct ref is slightly different
  res <- make_fake_resolution(
    `cran::crayon@>=1.0.0` = list(
      direct = TRUE
    )
  )

  ## Installed, but old
  bad6 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "crayon",
      version = "0.0.1",
      extra = list(list(repotype = "cran"))
    )
  )
  expect_false(ans <- satisfy_remote_cran(res, bad6))
  expect_match(attr(ans, "reason"), "Direct ref needs update")

  # without version req
  res <- make_fake_resolution(`cran::crayon` = list())

  ok5 <- make_fake_resolution(`cran::crayon` = list())
  expect_true(satisfy_remote_cran(res, ok5))
})

test_that("installedok", {
  sol <- list(
    package = "pkg1",
    version = "1.0.0",
    platform = "aarch64-apple-darwin20",
    built = "R 4.2.0; aarch64-apple-darwin20; 2022-09-13 21:32:41 UTC; unix"
  )

  inst <- list(
    package = "pkg1",
    version = "1.0.0",
    platform = "aarch64-apple-darwin20",
    built = "R 4.2.0; aarch64-apple-darwin20; 2022-09-13 21:32:41 UTC; unix",
    repository = "CRAN"
  )

  expect_true(installedok_remote_cran(inst, sol))
  expect_true(installedok_remote_cran(
    utils::modifyList(inst, list(platform = "*")),
    sol
  ))

  sol <- list(
    package = "pkg1",
    version = "1.0.0",
    platform = "source"
  )

  expect_true(installedok_remote_cran(inst, sol))
})
