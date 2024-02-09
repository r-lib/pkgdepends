
suppressMessages(type_github_builtin_token())

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  PKG_SYSREQS = "false",
  .local_envir = teardown_env()
)

# until the r -> bioc mapping is final for R 4.4.0
if (getRversion() >= "4.4.0") {
  withr::local_envvar(
    R_BIOC_VERSION = "3.17",
    .local_envir = teardown_env()
  )
}
