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

test_that("is_configure_opts", {
  expect_true(is_configure_opts(NULL))
  expect_true(is_configure_opts(character()))
  expect_true(is_configure_opts("--with-foo"))
  expect_true(is_configure_opts(c(pkgA = "--with-a", pkgB = "--with-b")))
  expect_true(is_configure_opts(list(pkgA = c("--a", "--b"))))

  expect_false(is_configure_opts(42))
  expect_false(is_configure_opts(NA_character_))
  expect_false(is_configure_opts(list(1, 2)))
})

test_that("env_decode_configure_opts", {
  # empty
  expect_equal(env_decode_configure_opts("", "PKG_CONFIGURE_ARGS"), character())

  # unnamed, applies to all packages (kept as a single string, even with `=`)
  expect_equal(
    env_decode_configure_opts("--with-foo=/opt/foo", "PKG_CONFIGURE_ARGS"),
    "--with-foo=/opt/foo"
  )

  # named, per-package (split on the first `=` of each `;`-separated part)
  expect_equal(
    env_decode_configure_opts(
      "curl=--with-curl=/usr/bin;xml2=--with-xml",
      "PKG_CONFIGURE_ARGS"
    ),
    c(curl = "--with-curl=/usr/bin", xml2 = "--with-xml")
  )
})

test_that("configure_flag", {
  # nothing
  expect_equal(configure_flag(NULL, "x", "--configure-args"), character())
  expect_equal(
    configure_flag(character(), "x", "--configure-args"),
    character()
  )

  # unnamed applies to any package, mirroring install.packages()
  expect_equal(
    configure_flag("--with-foo", "anything", "--configure-args"),
    paste0("--configure-args=", shQuote("--with-foo"))
  )
  # multiple unnamed elements are collapsed with a space
  expect_equal(
    configure_flag(c("--a", "--b"), "x", "--configure-vars"),
    paste0("--configure-vars=", shQuote("--a --b"))
  )

  # named: only the matching package gets the flag
  val <- c(pkgA = "--with-a", pkgB = "--with-b")
  expect_equal(
    configure_flag(val, "pkgA", "--configure-args"),
    paste0("--configure-args=", shQuote("--with-a"))
  )
  expect_equal(configure_flag(val, "pkgC", "--configure-args"), character())

  # named list of character vectors is supported too
  expect_equal(
    configure_flag(list(pkgA = c("--a", "--b")), "pkgA", "--configure-vars"),
    paste0("--configure-vars=", shQuote("--a --b"))
  )
})

test_that("configure_args / configure_vars config options", {
  # default falls back to the base `configure.args` / `configure.vars`
  # options, for compatibility with install.packages()
  withr::local_options(
    configure.args = "--with-x",
    configure.vars = c(pkg = "CC=clang")
  )
  cf <- current_config()
  expect_equal(cf$get("configure_args"), "--with-x")
  expect_equal(cf$get("configure_vars"), c(pkg = "CC=clang"))

  # explicit config value wins over the base option
  cf2 <- current_config()$update(list(configure_args = c(foo = "--bar")))
  expect_equal(cf2$get("configure_args"), c(foo = "--bar"))
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
