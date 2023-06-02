
test_that("sysreqs_platforms", {
  expect_snapshot(sysreqs_platforms())
})

test_that("sysreqs_db_list", {
  skip_on_cran()
  withr::local_options(pkg.sysreqs_db_update = FALSE)

  lst <- sysreqs_db_list("ubuntu-22.04")
  lst <- lst[lst$name %in% c("cairo", "chrome", "libcurl"), ]
  expect_snapshot({
    lst$packages
    lst$pre_install
    lst$post_install
  })
})

test_that("sysreqs_db_match", {
  skip_on_cran()
  withr::local_options(pkg.sysreqs_db_update = FALSE)

  res <- sysreqs_db_match(
    c("java and libcurl", "openssl would be good"),
    sysreqs_platform = "ubuntu-22.04"
  )
  expect_snapshot({
    res
    lapply(res, "[[", "packages")
    lapply(res, "[[", "post_install")
  })
})

test_that("sysreqs_install_plan", {
  skip_on_cran()
  setup_fake_apps()
  setup_fake_gh_app()
  withr::local_options(pkg.sysreqs_db_update = FALSE)

  res <- suppressMessages(sysreqs_install_plan(
    "curl",
    config = list(sysreqs_platform = "ubuntu-22.04")
  ))
  expect_snapshot({
    res
    res$packages$packages
    res$packages$system_packages
  })

  res2 <- suppressMessages(sysreqs_install_plan(
    "cran/rJava",
    config = list(sysreqs_platform = "centos-7")
  ))
  expect_snapshot({
    res2
    res2$packages$packages
    res2$packages$system_packages
  })
})

test_that("sysreqs_check_installed", {
  skip_on_cran()
  withr::local_options(pkg.sysreqs_db_update = FALSE)

  fakesys <- data_frame(
    status = "ii",
    package = c("gsfonts", "libcurl4-openssl-dev"),
    provides = list(
      character(),
      c("libcurl-dev",
        "libcurl-ssl-dev",
        "libcurl3-dev",
        "libcurl3-openssl-dev",
        "libcurl4-dev"
        )
    )
  )

  faker <- data_frame(
    Package = c("magick", "curl"),
    SystemRequirements = c(
      "ImageMagick++: ImageMagick-c++-devel (rpm) or\nlibmagick++-dev (deb)",
      "libcurl: libcurl-devel (rpm) or\nlibcurl4-openssl-dev (deb)."
    )
  )

  mockery::stub(
    sysreqs_check_installed,
    "async_system_list_packages",
    function(...) async_constant(fakesys)
  )
  mockery::stub(
    sysreqs_check_installed,
    "async_parse_installed",
    function(...) async_constant(faker)
  )

  withr::local_options(pkg.sysreqs_platform = "ubuntu-22.04")
  res <- sysreqs_check_installed()
  expect_snapshot(res)

  # can drop class
  expect_snapshot(res[])

  # edge case of no sysreqs at all
  faker <- data_frame(
    Package = c("filelock")
  )
  expect_snapshot(sysreqs_check_installed())
})

test_that("async_parse_installed", {
  mockery::stub(
    async_parse_installed, "pkgcache::parse_installed",
    data_frame(Package = c("foo", "bar"))
  )

  expect_snapshot(synchronize(async_parse_installed(
    library = .libPaths()[1],
    packages = c("foo", "bar", "baz")
  )))
})

test_that("parse_sysreqs_platform", {
  expect_snapshot({
    parse_sysreqs_platform("x86_64-pc-linux-gnu-ubuntu-22.04")
    parse_sysreqs_platform("x86_64-pc-linux-musl-alpine-3.14.1")
    parse_sysreqs_platform("x86_64-pc-linux-ubuntu-22.04")
    parse_sysreqs_platform("ubuntu-22.04")
    parse_sysreqs_platform("aarch64-apple-darwin20")
    parse_sysreqs_platform("i386+x86_64-w64-mingw32")
    parse_sysreqs_platform("x86_64-w64-mingw32")
    parse_sysreqs_platform("ubuntu")
    parse_sysreqs_platform("x86_64-px-linux-gnu-ubuntu-bar-baz")
  })
})

test_that("is_root", {
  mockery::stub(is_root, "os_type", "windows")
  expect_false(is_root())

  mockery::stub(is_root, "os_type", "unix")
  mockery::stub(is_root, "ps::ps_uids", c(effective = 0L))
  expect_true(is_root())

  mockery::stub(is_root, "ps::ps_uids", c(effective = 500L))
  expect_false(is_root())
})

test_that("can_sudo_without_pw", {
  mockery::stub(can_sudo_without_pw, "os_type", "windows")
  expect_false(can_sudo_without_pw())

  mockery::stub(can_sudo_without_pw, "os_type", "unix")
  mockery::stub(can_sudo_without_pw, "processx::run", list(status = 0))
  expect_true(can_sudo_without_pw())

  mockery::stub(can_sudo_without_pw, "processx::run", function(...) stop("no"))
  expect_false(can_sudo_without_pw())
})

# -------------------------------------------------------------------------
# Tests for the old implementation

test_that("query, post_install", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = "false")
  expect_snapshot({
    srq <- sysreqs_resolve("java", "ubuntu-22.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("pre_install", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = "false")
  expect_snapshot({
    srq <- sysreqs_resolve("this needs geos please", "ubuntu-16.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("multiple sysreqs", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = "false")
  expect_snapshot({
    srq <- sysreqs_resolve("java and also libcurl", "ubuntu-22.04")
    srq$total <- 1/3
    srq
  }, transform = transform_sysreqs_server)
})

test_that("error, unknown os", {
  setup_fake_sysreqs_app()
  withr::local_envvar(R_PKG_SYSREQS2 = "false")
  expect_snapshot(
    error = TRUE,
    transform = transform_sysreqs_server,
    sysreqs_resolve("java", "foobar-11")
  )
})

test_that("sysreqs_install", {
  skip_on_os("windows")
  setup_fake_sysreqs_app()
  withr::local_envvar(
    PKG_SYSREQS_DRY_RUN = "true",
    PKG_SYSREQS_SUDO = "false",
    R_PKG_SYSREQS2 = "false"
  )

  # not verbose
  withr::local_envvar(PKG_SYSREQS_VERBOSE = "false")
  srq <- sysreqs_resolve("libcurl and openssl", "ubuntu-22.04")
  expect_snapshot(sysreqs_install(srq))

  srq <- sysreqs_resolve("java and also libcurl", "ubuntu-22.04")
  expect_snapshot(sysreqs_install(srq))

  # verbose
  withr::local_envvar(PKG_SYSREQS_VERBOSE = "true")
  expect_snapshot(sysreqs_install(srq))

  # nothing to do
  expect_silent(expect_null(sysreqs_install(list())))
})

test_that("compact_cmds", {
  expect_snapshot({
    compact_cmds(character())
    compact_cmds(c(
      "apt-get install -y libssl-dev"
    ))
    compact_cmds(c(
      "apt-get install -y libssl-dev",
      "apt-get install -y libcurl4-openssl-dev"
    ))
  })
})

test_that("highlight_sysreqs", {
  # edge case
  expect_equal(highlight_sysreqs(NULL), "")

  sq <- list(
    NULL,
    list(
      list(
        sysreq = "fontconfig",
        packages = "libfontconfig1-dev",
        pre_install = NULL,
        post_install = NULL,
        packages_missing = character(0)
      ),
      list(
        sysreq = "freetype",
        packages = "libfreetype6-dev",
        pre_install = NULL,
        post_install = NULL,
        packages_missing = character(0)
      )
    ),
    list(
      list(
        sysreq = "freetype",
        packages = "libfreetype6-dev",
        pre_install = NULL,
        post_install = NULL,
        packages_missing = character(0)
      ),
      list(
        sysreq = "fribidi",
        packages = "libfribidi-dev",
        pre_install = NULL,
        post_install = NULL,
        packages_missing = "libfribidi-dev"
      ),
      list(
        sysreq = "harfbuzz",
        packages = "libharfbuzz-dev",
        pre_install = NULL,
        post_install = NULL,
        packages_missing = "libharfbuzz-dev"
      )
    ),
    NULL
  )
  sq2 <- lapply(sq, function(x) {
    lapply(x, function(xx) xx[setdiff(names(xx), "packages_missing")])
  })

  expect_snapshot({
    highlight_sysreqs(sq)
    highlight_sysreqs(sq2)
  })

  if (!l10n_info()[["UTF-8"]]) skip("No UTF-8 support")
  withr::local_options(cli.unicode = TRUE, cli.num_colors = 256)
  expect_snapshot({
    highlight_sysreqs(sq)
    highlight_sysreqs(sq2)
  })

  # installer
  sq[[2]] <- list(list(
    sysreq = "chrome",
    packages = NULL,
    pre_install = "download and install chrome",
    post_install = NULL,
    packages_missing = character()
  ))
  expect_snapshot({
    highlight_sysreqs(sq)
  })
})

test_that("default_sysreqs", {
  config <- current_config()
  config$set("sysreqs_platform", "aarch64-apple-darwin20")
  expect_false(default_sysreqs(config))

  config$set("sysreqs_platform", "x86_64-pc-linux-gnu-ubuntu-22.04")
  mockery::stub(default_sysreqs, "is_root", FALSE)
  mockery::stub(default_sysreqs, "can_sudo_without_pw", FALSE)
  expect_false(default_sysreqs(config))

  mockery::stub(default_sysreqs, "is_root", TRUE)
  mockery::stub(default_sysreqs, "can_sudo_without_pw", FALSE)
  expect_true(default_sysreqs(config))

  mockery::stub(default_sysreqs, "is_root", FALSE)
  mockery::stub(default_sysreqs, "can_sudo_without_pw", TRUE)
  expect_true(default_sysreqs(config))
})
