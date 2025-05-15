test_that("sysreqs_is_supported", {
  expect_true(sysreqs_is_supported("ubuntu-20.04"))
  expect_true(sysreqs_is_supported("ubuntu-22.04"))
  expect_true(sysreqs_is_supported("ubuntu-24.04"))

  expect_true(sysreqs_is_supported("debian-11"))
  expect_true(sysreqs_is_supported("debian-12"))
  expect_true(sysreqs_is_supported("debian-unstable"))

  expect_true(sysreqs_is_supported("opensuse-15.4"))
  expect_true(sysreqs_is_supported("opensuse-15.5"))
  expect_true(sysreqs_is_supported("opensuse-15.6"))
  expect_true(sysreqs_is_supported("opensuse-leap-15.4"))
  expect_true(sysreqs_is_supported("opensuse-leap-15.5"))
  expect_true(sysreqs_is_supported("opensuse-leap-15.6"))
  expect_true(sysreqs_is_supported("opensuse-tumbleweed-20250509"))

  expect_true(sysreqs_is_supported("centos-6"))
  expect_true(sysreqs_is_supported("centos-7"))
  expect_true(sysreqs_is_supported("centos-8"))

  expect_true(sysreqs_is_supported("rhel-7"))
  expect_true(sysreqs_is_supported("rhel-7.9"))
  expect_true(sysreqs_is_supported("rhel-8"))
  expect_true(sysreqs_is_supported("rhel-8.10"))
  expect_true(sysreqs_is_supported("rhel-9"))
  expect_true(sysreqs_is_supported("rhel-9.6"))

  expect_true(sysreqs_is_supported("fedora-39"))
  expect_true(sysreqs_is_supported("fedora-40"))
  expect_true(sysreqs_is_supported("fedora-41"))
  expect_true(sysreqs_is_supported("fedora-42"))
  expect_true(sysreqs_is_supported("fedora-43"))

  expect_true(sysreqs_is_supported("sles-15.4"))
  expect_true(sysreqs_is_supported("sles-15.5"))
  expect_true(sysreqs_is_supported("sles-15.6"))

  expect_true(sysreqs_is_supported("almalinux-8.10"))
  expect_true(sysreqs_is_supported("almalinux-9.6"))

  expect_true(sysreqs_is_supported("rocky-8.9"))
  expect_true(sysreqs_is_supported("rocky-9.3"))

  expect_true(sysreqs_is_supported("alpine-3.19"))
  expect_true(sysreqs_is_supported("alpine-3.20"))
  expect_true(sysreqs_is_supported("alpine-3.21"))
  expect_true(sysreqs_is_supported("alpine-edge"))

  # these should not happen, these are the PPM names
  expect_true(sysreqs_is_supported("redhat-8"))
  expect_true(sysreqs_is_supported("redhat-8.10"))
  expect_true(sysreqs_is_supported("rockylinux-8"))
  expect_true(sysreqs_is_supported("rockylinux-8.9"))

  expect_false(sysreqs_is_supported("foo-bar"))
})

test_that("update", {
  # TODO: needs internet
  skip_on_cran()
  tmp <- withr::local_tempdir()
  synchronize(sysreqs2_async_update_metadata(tmp))
  expect_true(file.exists(tmp))
  expect_true("rules" %in% dir(tmp))

  # no update if already there
  synchronize(sysreqs2_async_update_metadata(tmp))
  expect_true(file.exists(tmp))
  expect_true("rules" %in% dir(tmp))
})

test_that("match", {
  skip_on_cran()
  sr1 <- sysreqs2_resolve("libcurl and Java", "ubuntu-22.04")
  expect_snapshot(sr1)

  sr2 <- sysreqs2_resolve("libcurl and Java", "debian-unstable")
  expect_snapshot(sr2)
})

test_that("sysreqs2_command error", {
  expect_snapshot(
    error = TRUE,
    sysreqs2_command("foobar-2023")
  )
})

test_that("do not run update if nothing to do", {
  skip_on_cran()
  sr1 <- sysreqs2_resolve("nothing needed", "ubuntu-22.04")
  expect_snapshot(sr1)
})
