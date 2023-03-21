
test_that("sysreqs2_is_supported", {
  expect_true(sysreqs2_is_supported("ubuntu", "22.04"))
  expect_true(sysreqs2_is_supported("debian", "unstable"))
  expect_true(sysreqs2_is_supported("centos", "8"))
  expect_true(sysreqs2_is_supported("rockylinux", "9"))
  expect_true(sysreqs2_is_supported("redhat", "8"))
  expect_true(sysreqs2_is_supported("opensuse", "15.4"))
  expect_true(sysreqs2_is_supported("sle", "15.4"))

  expect_false(sysreqs2_is_supported("foo", "bar"))
})

test_that("update", {
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
  sr1 <- sysreqs2_resolve("libcurl and Java", "ubuntu", "22.04")
  sr1$total <- 0.05
  expect_snapshot(sr1)

  sr2 <- sysreqs2_resolve("libcurl and Java", "debian", "unstable")
  sr2$total <- 0.05
  expect_snapshot(sr2)
})

test_that("sysreqs2_command error", {
  expect_snapshot(
    error = TRUE,
    sysreqs2_command("foobar", "2023")
  )
})

test_that("do not run update if nothing to do", {
  skip_on_cran()
  sr1 <- sysreqs2_resolve("nothing needed", "ubuntu", "22.04")
  sr1$total <- 0.05
  expect_snapshot(sr1)
})
