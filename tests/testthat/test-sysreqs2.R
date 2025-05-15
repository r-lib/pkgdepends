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

test_that("sysreqs_platforms", {
  skip_on_cran()
  sysreqs <- function(sysreqs_platform) {
    sysreqs_resolve("libxml2", sysreqs_platform)[["install_scripts"]]
  }
  expect_snapshot({
    sysreqs("ubuntu-20.04")
    sysreqs("ubuntu-22.04")
    sysreqs("ubuntu-24.04")

    sysreqs("debian-11")
    sysreqs("debian-12")
    sysreqs("debian-unstable")

    sysreqs("opensuse-15.4")
    sysreqs("opensuse-15.5")
    sysreqs("opensuse-15.6")
    sysreqs("opensuse-leap-15.4")
    sysreqs("opensuse-leap-15.5")
    sysreqs("opensuse-leap-15.6")
    sysreqs("opensuse-tumbleweed-20250509")

    sysreqs("centos-6")
    sysreqs("centos-7")
    sysreqs("centos-8")

    sysreqs("rhel-7")
    sysreqs("rhel-7.9")
    sysreqs("rhel-8")
    sysreqs("rhel-8.10")
    sysreqs("rhel-9")
    sysreqs("rhel-9.6")

    sysreqs("fedora-39")
    sysreqs("fedora-40")
    sysreqs("fedora-42")
    sysreqs("fedora-42")
    sysreqs("fedora-43")

    sysreqs("sles-15.4")
    sysreqs("sles-15.5")
    sysreqs("sles-15.6")

    sysreqs("almalinux-8.10")
    sysreqs("almalinux-9.6")

    sysreqs("rocky-8.9")
    sysreqs("rocky-9.3")

    sysreqs("alpine-3.19")
    sysreqs("alpine-3.20")
    sysreqs("alpine-3.21")
    sysreqs("alpine-3.edge")

    sysreqs("redhat-8")
    sysreqs("redhat-8.10")
    sysreqs("rockylinux-8")
    sysreqs("rockylinux-8.9")
  })
})

test_that("canonize_sysreqs_platform", {
  expect_snapshot({
    canonize_sysreqs_platform("ubuntu-20.04")
    canonize_sysreqs_platform("ubuntu-22.04")
    canonize_sysreqs_platform("ubuntu-24.04")

    canonize_sysreqs_platform("debian-11")
    canonize_sysreqs_platform("debian-12")
    canonize_sysreqs_platform("debian-unstable")

    canonize_sysreqs_platform("opensuse-15.4")
    canonize_sysreqs_platform("opensuse-15.5")
    canonize_sysreqs_platform("opensuse-15.6")
    canonize_sysreqs_platform("opensuse-leap-15.4")
    canonize_sysreqs_platform("opensuse-leap-15.5")
    canonize_sysreqs_platform("opensuse-leap-15.6")
    canonize_sysreqs_platform("opensuse-tumbleweed-20250509")

    canonize_sysreqs_platform("centos-6")
    canonize_sysreqs_platform("centos-7")
    canonize_sysreqs_platform("centos-8")

    canonize_sysreqs_platform("rhel-7")
    canonize_sysreqs_platform("rhel-7.9")
    canonize_sysreqs_platform("rhel-8")
    canonize_sysreqs_platform("rhel-8.10")
    canonize_sysreqs_platform("rhel-9")
    canonize_sysreqs_platform("rhel-9.6")

    canonize_sysreqs_platform("fedora-39")
    canonize_sysreqs_platform("fedora-40")
    canonize_sysreqs_platform("fedora-42")
    canonize_sysreqs_platform("fedora-42")
    canonize_sysreqs_platform("fedora-43")

    canonize_sysreqs_platform("sles-15.4")
    canonize_sysreqs_platform("sles-15.5")
    canonize_sysreqs_platform("sles-15.6")

    canonize_sysreqs_platform("almalinux-8.10")
    canonize_sysreqs_platform("almalinux-9.6")

    canonize_sysreqs_platform("rocky-8.9")
    canonize_sysreqs_platform("rocky-9.3")

    canonize_sysreqs_platform("alpine-3.19")
    canonize_sysreqs_platform("alpine-3.20")
    canonize_sysreqs_platform("alpine-3.21")
    canonize_sysreqs_platform("alpine-3.19.1")
    canonize_sysreqs_platform("alpine-3.20.2")
    canonize_sysreqs_platform("alpine-3.21.3")
    canonize_sysreqs_platform("alpine-3.edge")

    canonize_sysreqs_platform("redhat-8")
    canonize_sysreqs_platform("redhat-8.10")
    canonize_sysreqs_platform("rockylinux-8")
    canonize_sysreqs_platform("rockylinux-8.9")
  })
})
