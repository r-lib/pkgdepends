test_that("current_config runs without error", {
  expect_no_error(current_config())
})

test_that("current_config runs without error when difftime config set", {
  withr::local_envvar(PKG_SYSREQS_DB_UPDATE_TIMEOUT = "30")
  expect_error(
    current_config()$get("sysreqs_db_update_timeout"),
    "Invalid time interval specification"
  )
  withr::local_envvar(PKG_SYSREQS_DB_UPDATE_TIMEOUT = "30s")
  expect_equal(
    current_config()$get("sysreqs_db_update_timeout"),
    as.difftime(30, units = 'secs'),
    ignore_attr = TRUE
  )
  withr::local_envvar(PKG_SYSREQS_DB_UPDATE_TIMEOUT = "30h")
  expect_equal(
    current_config()$get("sysreqs_db_update_timeout"),
    as.difftime(30, units = "hours"),
    ignore_attr = TRUE
  )
})
stopifnot(Sys.getenv('PKG_SYSREQS_DB_UPDATE_TIMEOUT') == '')