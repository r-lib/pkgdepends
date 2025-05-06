test_that("parse_remote", {
  pr <- parse_pkg_refs("bioc::Biobase")[[1]]
  expect_equal(pr$package, "Biobase")
  expect_equal(pr$atleast, "")
  expect_equal(pr$version, "")
  expect_equal(pr$ref, "bioc::Biobase")
  expect_equal(pr$type, "bioc")
  expect_true("remote_ref_bioc" %in% class(pr))
  expect_true("remote_ref" %in% class(pr))
})

test_that("resolve_remote", {
  setup_fake_apps()

  conf <- current_config()
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  res <- suppressMessages(synchronise(resolve_remote_bioc(
    parse_pkg_ref("bioc::Biobase"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  res$md5sum <- "<md5sum>"

  # If this fails then we need to update bioc-config.yaml in pkgcache
  expect_snapshot(
    as.list(res),
    transform = function(x) transform_local_port(transform_bioc_version(x)),
    variant = if (getRversion() < "3.6.0") "old-r" else "new-r"
  )
})

test_that("failed resolution", {
  setup_fake_apps()

  conf <- current_config()
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  res <- suppressMessages(synchronise(resolve_remote_bioc(
    parse_pkg_ref("bioc::nosuchpackageinbiocforsure"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  expect_true(all(res$status == "FAILED"))

  expect_snapshot(
    as.list(res),
    transform = function(x) transform_local_port(transform_bioc_version(x)),
    variant = paste0("pillar-", packageVersion("pillar"))
  )
})

test_that("download_remote", {
  setup_fake_apps()

  dir.create(tmp <- tempfile())
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  conf <- current_config()
  conf$platforms <- "source"
  conf$cache_dir <- tmp
  conf$package_cache_dir <- tmp2
  cache <- list(
    package = pkgcache::package_cache$new(conf$package_cache_dir),
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  res <- suppressMessages(synchronise(resolve_remote_bioc(
    parse_pkg_ref("bioc::Biobase"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-t")
  dl <- synchronise(download_remote_bioc(
    res[1, ],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))

  expect_equal(dl, "Got")
  expect_true(file.exists(target))

  unlink(target)
  dl2 <- synchronise(download_remote_bioc(
    res[1, ],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))
  expect_true(dl2 %in% c("Had", "Current"))
  expect_true(file.exists(target))
})

test_that("satisfies_remote", {
  res <- make_fake_resolution(`bioc::eisa@>=1.0.0` = list())

  ## GitHub type is never good
  bad1 <- make_fake_resolution(`github::r-lib/eisa` = list())
  expect_false(ans <- satisfy_remote_bioc(res, bad1))
  expect_match(attr(ans, "reason"), "Type must be")

  ## Missing DESCRIPTION for installed type
  bad2 <- make_fake_resolution(`installed::foobar` = list())
  expect_false(ans <- satisfy_remote_bioc(res, bad2))
  expect_match(attr(ans, "reason"), "not from BioC")

  ## installed, but not from BioC
  fake_desc <- desc::desc("!new")
  bad3 <- make_fake_resolution(
    `installed::foobar` = list(
      extra = list(list(description = fake_desc))
    )
  )
  expect_false(ans <- satisfy_remote_bioc(res, bad3))
  expect_match(attr(ans, "reason"), "not from BioC")

  ## BioC type, but package name does not match
  bad4 <- make_fake_resolution(`bioc::eisa2` = list())
  expect_false(ans <- satisfy_remote_bioc(res, bad4))
  expect_match(attr(ans, "reason"), "names differ")

  ## installed type, but package name does not match
  bad5 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "eisa2",
      extra = list(list(repotype = "bioc"))
    )
  )
  expect_false(ans <- satisfy_remote_bioc(res, bad5))
  expect_match(attr(ans, "reason"), "names differ")

  ## BioC type, but version is not good enough
  bad6 <- make_fake_resolution(`bioc::eisa` = list(version = "0.0.1"))
  expect_false(ans <- satisfy_remote_bioc(res, bad6))
  expect_match(attr(ans, "reason"), "Insufficient version")

  ## Same version, BioC
  ok1 <- make_fake_resolution(`bioc::eisa` = list())
  expect_true(satisfy_remote_bioc(res, ok1))

  ## Newer version, BioC
  ok2 <- make_fake_resolution(`bioc::eisa` = list(version = "2.0.0"))
  expect_true(satisfy_remote_bioc(res, ok2))

  ## Same version, installed
  ok3 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "eisa",
      extra = list(list(repotype = "bioc"))
    )
  )
  expect_true(satisfy_remote_bioc(res, ok3))

  ## Newer version, installed
  ok4 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "eisa",
      version = "2.0.0",
      extra = list(list(repotype = "bioc"))
    )
  )
  expect_true(satisfy_remote_bioc(res, ok4))
})

test_that("satisfies_remote 2", {
  res <- make_fake_resolution(`bioc::eisa@>=1.0.0` = list(direct = TRUE))

  bad1 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "eisa",
      version = "0.0.1",
      extra = list(list(repotype = "bioc"))
    )
  )
  expect_false(satisfy_remote_bioc(res, bad1))

  res2 <- make_fake_resolution(`bioc::eisa` = list())

  ok1 <- make_fake_resolution(
    `installed::foobar` = list(
      package = "eisa",
      version = "0.0.1",
      extra = list(list(repotype = "bioc"))
    )
  )
  expect_true(satisfy_remote_bioc(res2, ok1))
})

test_that("installedok", {
  # Bioc binary
  sol <- list(
    repotype = "bioc",
    package = "package",
    version = "1.0.0",
    platform = "aarch64-apple-darwin20",
    built = "R 4.2.0; aarch64-apple-darwin20; 2022-10-08 08:37:45 UTC; unix"
  )
  expect_true(installedok_remote_standard(
    list(
      repotype = "cran",
      package = "package",
      version = "1.0.0",
      platform = "aarch64-apple-darwin20",
      built = "R 4.2.0; aarch64-apple-darwin20; 2022-10-08 08:37:45 UTC; unix"
    ),
    sol
  ))
  expect_false(installedok_remote_standard(
    list(
      package = "package",
      version = "1.0.0",
      platform = "aarch64-apple-darwin20",
      built = "R 4.2.0; aarch64-apple-darwin20; 2022-11-08 08:37:45 UTC; unix"
    ),
    sol
  ))

  # Bioc source
  sol <- list(
    repotype = "bioc",
    package = "package",
    version = "1.0.0",
    platform = "source"
  )
  expect_true(installedok_remote_standard(
    list(
      package = "package",
      version = "1.0.0",
      repotype = "bioc"
    ),
    sol
  ))
  expect_false(installedok_remote_standard(
    list(
      package = "package",
      version = "1.0.0"
    ),
    sol
  ))
})
