
test_that("query, post_install", {
  setup_fake_sysreqs_app()
  expect_snapshot({
    srq <- sysreqs_resolve("java", "ubuntu", "22.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("pre_install", {
  setup_fake_sysreqs_app()
  expect_snapshot({
    srq <- sysreqs_resolve("this needs geos please", "ubuntu", "16.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("multiple sysreqs", {
  setup_fake_sysreqs_app()
  expect_snapshot({
    srq <- sysreqs_resolve("java and also libcurl", "ubuntu", "22.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("system is detected", {
  setup_fake_sysreqs_app()
  mockery::stub(
    sysreqs_resolve,
    "detect_linux",
    list(distribution = "ubuntu", release = "22.04")
  )
  expect_snapshot({
    srq <- sysreqs_resolve("java and also libcurl")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("error, unknown os", {
  setup_fake_sysreqs_app()
  expect_snapshot(
    error = TRUE,
    transform = transform_sysreqs_server,
    sysreqs_resolve("java", "debian", "11")
  )
})

test_that("sysreqs_install", {
  skip_on_os("windows")
  setup_fake_sysreqs_app()
  withr::local_envvar(
    PKG_SYSREQS_DRY_RUN = "true",
    PKG_SYSREQS_SUDO = "false"
  )

  # not verbose
  withr::local_envvar(PKG_SYSREQS_VERBOSE = "false")
  srq <- sysreqs_resolve("java and also libcurl", "ubuntu", "22.04")
  expect_snapshot(sysreqs_install(srq))

  # verbose
  withr::local_envvar(PKG_SYSREQS_VERBOSE = "true")
  expect_snapshot(sysreqs_install(srq))

  # nothing to do
  expect_silent(expect_null(sysreqs_install(list())))
})

test_that("detect_linux", {
  mockery::stub(
    detect_linux,
    "pkgcache::current_r_platform_data",
    list(distribution = "ubuntu", release = "22.04")
  )
  expect_snapshot(detect_linux())

  mockery::stub(
    detect_linux,
    "pkgcache::current_r_platform_data",
    list()
  )
  expect_snapshot(detect_linux())
})
