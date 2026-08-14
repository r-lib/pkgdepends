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
  # Use a local git server that serves the bundled rules, so we don't need
  # to access the real r-system-requirements repository over the internet.
  skip_on_cran()
  setup_fake_sysreqs_git()
  local_fake_git_no_creds(fake_sysreqs_git$url())
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
  # Resolve against the bundled rules, without updating from the internet.
  config <- current_config()$set("sysreqs_db_update", FALSE)
  sr1 <- sysreqs2_resolve("libcurl and Java", "ubuntu-22.04", config = config)
  expect_snapshot(sr1)

  sr2 <- sysreqs2_resolve(
    "libcurl and Java",
    "debian-unstable",
    config = config
  )
  expect_snapshot(sr2)
})

test_that("sysreqs2_command error", {
  expect_snapshot(
    error = TRUE,
    sysreqs2_command("foobar-2023")
  )
})

test_that("do not run update if nothing to do", {
  config <- current_config()$set("sysreqs_db_update", FALSE)
  sr1 <- sysreqs2_resolve("nothing needed", "ubuntu-22.04", config = config)
  expect_snapshot(sr1)
})

test_that("sysreqs_platforms", {
  config <- current_config()$set("sysreqs_db_update", FALSE)
  sysreqs <- function(sysreqs_platform) {
    sysreqs_resolve(
      "libxml2",
      sysreqs_platform,
      config = config
    )[["install_scripts"]]
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

test_that("sysreqs2_scripts deduplicates repeated pre/post_install commands", {
  # https://github.com/r-lib/pak/issues/888
  rust <- paste0(
    "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | ",
    "sh -s -- --no-modify-path -y"
  )
  post <- "echo done"
  recs <- list(
    list(list(pre_install = rust, post_install = post, packages = "pkgA")),
    list(list(pre_install = rust, post_install = post, packages = "pkgB"))
  )
  scripts <- sysreqs2_scripts(recs, "ubuntu-24.04")
  expect_equal(sum(scripts$pre_install == rust), 1L)
  expect_equal(scripts$post_install, post)
  expect_true(rust %in% scripts$pre_install)
})

test_that("is_ppm_manylinux_repo", {
  expect_true(is_ppm_manylinux_repo(
    "https://p3m.dev/cran/__linux__/manylinux_2_28/latest"
  ))
  # trailing slash, dated snapshot, other PPM host, future manylinux version
  expect_true(is_ppm_manylinux_repo(
    "https://p3m.dev/cran/__linux__/manylinux_2_28/latest/"
  ))
  expect_true(is_ppm_manylinux_repo(
    "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/2026-01-01"
  ))
  expect_true(is_ppm_manylinux_repo(
    "https://p3m.dev/cran/__linux__/manylinux_2_34/latest"
  ))

  # a regular PPM Linux repo, a plain CRAN-like repo, and no repo at all
  expect_false(is_ppm_manylinux_repo(
    "https://p3m.dev/cran/__linux__/noble/latest"
  ))
  expect_false(is_ppm_manylinux_repo("https://cran.r-project.org"))
  expect_false(is_ppm_manylinux_repo(NA_character_))

  # vectorized, including the zero length case
  expect_equal(
    is_ppm_manylinux_repo(c(
      "https://p3m.dev/cran/__linux__/manylinux_2_28/latest",
      "https://p3m.dev/cran/__linux__/jammy/latest",
      NA_character_
    )),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(is_ppm_manylinux_repo(character()), logical())
})

test_that("binary_needs_no_sysreqs", {
  ppm <- "https://p3m.dev/cran/__linux__/manylinux_2_28/latest"
  expect_true(binary_needs_no_sysreqs("x86_64-pc-linux-gnu", ppm))
  # PPM uses an empty platform string for packages that need no compilation
  expect_true(binary_needs_no_sysreqs("", ppm))

  # source packages from the same repo do need their system requirements
  expect_false(binary_needs_no_sysreqs("source", ppm))
  # binaries from anywhere else are unaffected
  expect_false(binary_needs_no_sysreqs(
    "x86_64-pc-linux-gnu",
    "https://p3m.dev/cran/__linux__/noble/latest"
  ))
  expect_false(binary_needs_no_sysreqs("x86_64-pc-linux-gnu", NA_character_))
  expect_false(binary_needs_no_sysreqs(NA_character_, ppm))

  # a manylinux platform is enough, even if the repo URL does not look like
  # a manylinux repo
  expect_true(binary_needs_no_sysreqs(
    "x86_64-pc-linux-gnu-manylinux-2.28",
    "https://p3m.dev/cran/latest"
  ))
  expect_true(binary_needs_no_sysreqs(
    "x86_64-pc-linux-gnu-manylinux-2.28",
    NA_character_
  ))

  expect_equal(binary_needs_no_sysreqs(character(), character()), logical())
})

test_that("installed_needs_no_sysreqs", {
  lib <- withr::local_tempdir()
  dir.create(file.path(lib, "curl", "libs", ".libs"), recursive = TRUE)
  dir.create(file.path(lib, "magick", "libs"), recursive = TRUE)

  expect_true(installed_needs_no_sysreqs("curl", "RSPM", lib))
  # no bundled libraries
  expect_false(installed_needs_no_sysreqs("magick", "RSPM", lib))
  # not from PPM
  expect_false(installed_needs_no_sysreqs("curl", "CRAN", lib))
  expect_false(installed_needs_no_sysreqs("curl", NA_character_, lib))
  # not installed at all
  expect_false(installed_needs_no_sysreqs("filelock", "RSPM", lib))

  # vectorized, possibly over multiple libraries
  expect_equal(
    installed_needs_no_sysreqs(
      c("curl", "magick", "curl"),
      c("RSPM", "RSPM", "CRAN"),
      c(lib, lib, lib)
    ),
    c(TRUE, FALSE, FALSE)
  )

  # missing `Repository` or `LibPath` column, and the zero length case
  expect_equal(installed_needs_no_sysreqs("curl", NULL, lib), FALSE)
  expect_equal(installed_needs_no_sysreqs("curl", "RSPM", NULL), FALSE)
  expect_equal(
    installed_needs_no_sysreqs(character(), character(), character()),
    logical()
  )
})
