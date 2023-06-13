
test_that("async_system_list_packages, deb", {
  config <- current_config()
  config$set("sysreqs_platform", "x86_64-pc-linux-gnu-ubuntu-22.04")
  mockery::stub(
    async_system_list_packages,
    "async_system_list_packages_dpkg_query",
    function(...) async_constant("OK")
  )
  expect_equal(
    synchronise(async_system_list_packages(config)),
    "OK"
  )
})

test_that("async_system_list_packages, rpm", {
  config <- current_config()
  config$set("sysreqs_platform", "x86_64-pc-linux-gnu-redhat-9")
  mockery::stub(
    async_system_list_packages,
    "async_system_list_packages_rpm",
    function(...) async_constant("OK")
  )
  expect_equal(
    synchronise(async_system_list_packages(config)),
    "OK"
  )
})

test_that("async_system_list_packages_dpkg_query", {
  fake <- read_all(test_path("fixtures", "dpkg-query.txt"))
  mockery::stub(
    async_system_list_packages_dpkg_query,
    "external_process",
    function(...) async_constant(list(stdout = fake))
  )
  ans <- synchronise(async_system_list_packages_dpkg_query())
  expect_snapshot(ans)
})

test_that("async_system_list_packages_rpm", {
  fake <- read_all(test_path("fixtures", "rpm.txt"))
  mockery::stub(
    async_system_list_packages_rpm,
    "external_process",
    function(...) async_constant(list(stdout = fake))
  )
  ans <- synchronise(async_system_list_packages_rpm())
  expect_snapshot(ans)
})
