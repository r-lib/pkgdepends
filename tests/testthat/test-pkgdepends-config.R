test_that("windows_archs", {
  expect_snapshot(windows_archs())
})

test_that("default_windows_archs", {
  mockery::stub(default_windows_archs, "getRversion", package_version("4.1.0"))
  expect_equal(default_windows_archs(), "both")

  mockery::stub(default_windows_archs, "getRversion", package_version("4.2.0"))
  expect_equal(default_windows_archs(), "prefer-x64")
})

test_that("default_update_after", {
  expect_true(length(default_update_after()) == 1)
  expect_true(inherits(default_update_after(), "difftime"))
})

test_that("env_decode_dependencies", {
  expect_true(env_decode_dependencies("yes"))
  expect_true(env_decode_dependencies("true"))
  expect_true(env_decode_dependencies("1"))
  expect_true(env_decode_dependencies("on"))

  expect_false(env_decode_dependencies("no"))
  expect_false(env_decode_dependencies("false"))
  expect_false(env_decode_dependencies("0"))
  expect_false(env_decode_dependencies("off"))

  expect_equal(env_decode_dependencies("na"), NA)

  expect_equal(env_decode_dependencies("foo"), "foo")
  expect_equal(env_decode_dependencies("foo;bar"), c("foo", "bar"))
})

test_that("env_decode_difftime", {
  expect_snapshot({
    env_decode_difftime("0s")
    env_decode_difftime("120s")
    env_decode_difftime("0m")
    env_decode_difftime("120m")
    env_decode_difftime("0h")
    env_decode_difftime("24h")
    env_decode_difftime("0d")
    env_decode_difftime("1d")
  })

  expect_snapshot(error = TRUE, env_decode_difftime("", "PKG_UPDATE"))
  expect_snapshot(error = TRUE, env_decode_difftime("123", "PKG_UPDATE"))
  expect_snapshot(error = TRUE, env_decode_difftime("1k", "PKG_UPDATE"))
  expect_snapshot(error = TRUE, env_decode_difftime("k1k", "PKG_UPDATE"))
})

test_that("default_sysreqs_sudo", {
  mockery::stub(default_sysreqs_sudo, "os_type", "windows")
  expect_false(default_sysreqs_sudo())

  mockery::stub(default_sysreqs_sudo, "os_type", "unix")
  mockery::stub(default_sysreqs_sudo, "get_euid", 0L)
  expect_false(default_sysreqs_sudo())

  mockery::stub(default_sysreqs_sudo, "get_euid", 100L)
  expect_true(default_sysreqs_sudo())

  mockery::stub(default_sysreqs_sudo, "get_euid", NA_integer_)
  expect_true(default_sysreqs_sudo())
})

test_that("default_sysreqs_verbose", {
  withr::local_envvar(CI = "true")
  expect_true(default_sysreqs_verbose())

  withr::local_envvar(CI = NA_character_)
  expect_false(default_sysreqs_verbose())
})

test_that("default_sysreqs_rspm_url", {
  withr::local_envvar(RSPM_ROOT = "https://custom")
  expect_equal(default_sysreqs_rspm_url(), "https://custom")

  withr::local_envvar(RSPM_ROOT = NA_character_)
  expect_equal(
    default_sysreqs_rspm_url(),
    "https://packagemanager.posit.co"
  )
})

test_that("default_sysreqs_rspm_repo_id", {
  withr::local_envvar(RSPM_REPO_ID = "2")
  expect_equal(default_sysreqs_rspm_repo_id(), "2")

  withr::local_envvar(RSPM_REPO_ID = NA_character_)
  expect_equal(default_sysreqs_rspm_repo_id(), "1")
})

test_that("current_config", {
  expect_snapshot(
    sort(current_config()$list())
  )
})
