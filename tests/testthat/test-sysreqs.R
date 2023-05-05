
test_that("query, post_install", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = NA_character_)
  expect_snapshot({
    srq <- sysreqs_resolve("java", "ubuntu", "22.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("pre_install", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = NA_character_)
  expect_snapshot({
    srq <- sysreqs_resolve("this needs geos please", "ubuntu", "16.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("multiple sysreqs", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = NA_character_)
  expect_snapshot({
    srq <- sysreqs_resolve("java and also libcurl", "ubuntu", "22.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("error, unknown os", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = NA_character_)
  expect_snapshot(
    error = TRUE,
    transform = transform_sysreqs_server,
    sysreqs_resolve("java", "foobar", "11")
  )
})

test_that("sysreqs_install", {
  skip_on_os("windows")
  setup_fake_sysreqs_app()
  withr::local_envvar(
    PKG_SYSREQS_DRY_RUN = "true",
    PKG_SYSREQS_SUDO = "false",
    R_PKG_SYSREQS2 = NA_character_
  )

  # not verbose
  withr::local_envvar(PKG_SYSREQS_VERBOSE = "false")
  srq <- sysreqs_resolve("libcurl and openssl", "ubuntu", "22.04")
  expect_snapshot(sysreqs_install(srq))

  srq <- sysreqs_resolve("java and also libcurl", "ubuntu", "22.04")
  expect_snapshot(sysreqs_install(srq))

  # verbose
  withr::local_envvar(PKG_SYSREQS_VERBOSE = "true")
  expect_snapshot(sysreqs_install(srq))

  # nothing to do
  expect_silent(expect_null(sysreqs_install(list())))
})

test_that("compact_cmds", {
  expect_snapshot({
    compact_cmds(character())
    compact_cmds(c(
      "apt-get install -y libssl-dev"
    ))
    compact_cmds(c(
      "apt-get install -y libssl-dev",
      "apt-get install -y libcurl4-openssl-dev"
    ))
  })
})
