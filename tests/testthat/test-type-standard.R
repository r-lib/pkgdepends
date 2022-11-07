
test_that("resolve_remote", {
  setup_fake_apps()

  cols <- c("package", "version", "needscompilation",
            "repodir", "rversion", "platform", "priority", "ref", "type",
            "status", "target", "mirror", "sources", "sysreqs", "deps",
            "built", "repotype", "direct", "params", "metadata")

  conf <- current_config()
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache()
  )

  # CRAN package as standard package --------------------------------------

  res <- suppressMessages(synchronise(resolve_remote_standard(
    parse_pkg_ref("crayon"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  expect_snapshot(
    as.list(res[, intersect(names(res), cols)]),
    transform = transform_local_port
  )

  # Bioc package as standard package --------------------------------------

  res <- suppressMessages(synchronise(resolve_remote_standard(
    parse_pkg_ref("Biobase"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  expect_snapshot(
    as.list(res[, intersect(names(res), cols)]),
    transform = function(x) transform_local_port(transform_bioc_version(x)),
    variant = if (getRversion() < "3.6.0") "old-r" else "new-r"
  )

  # Proper error for non-existing package ---------------------------------

  nonpkg <- "thispackagecannotexistforsure"
  res <- suppressMessages(synchronise(resolve_remote_standard(
    parse_pkg_ref(nonpkg),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  expect_snapshot(
    as.list(res[, intersect(names(res), cols)]),
    transform = transform_local_port
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

  # CRAN package ----------------------------------------------------------

  res <- suppressMessages(synchronise(resolve_remote_standard(
    parse_pkg_ref("crayon"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-t")
  dl <- synchronise(download_remote_standard(
    res[1,],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))

  expect_equal(dl, "Got")
  expect_true(file.exists(target))
  unlink(target)

  # bioc package ----------------------------------------------------------

  res <- suppressMessages(synchronise(resolve_remote_standard(
    parse_pkg_ref("Biobase"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-t")
  dl <- synchronise(download_remote_standard(
    res[1,],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))

  expect_equal(dl, "Got")
  expect_true(file.exists(target))
  unlink(target)

  # caching ---------------------------------------------------------------

  dl2 <- synchronise(download_remote_standard(
    res[1,],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))
  expect_true(dl2 %in% c("Had", "Current"))
  expect_true(file.exists(target))
})

test_that("download cran-like repos", {
  setup_fake_apps()

  x_app_pkgs <- dcf("
    Package: foobar
    Version: 1.0.0
  ")

  fake_cranlike <- webfakes::local_app_process(cran_app(x_app_pkgs))
  withr::local_options(
    repos = c(X = fake_cranlike$url(), getOption("repos"))
  )

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

  res <- suppressMessages(synchronise(resolve_remote_standard(
    parse_pkg_ref("foobar"),
    TRUE,
    conf,
    cache,
    dependencies = FALSE
  )))

  target <- file.path(conf$cache_dir, res$target[1])
  tree <- paste0(target, "-t")
  dl <- synchronise(download_remote_standard(
    res[1,],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))

  expect_equal(dl, "Got")
  expect_true(file.exists(target))
  unlink(target)

  dl2 <- synchronise(download_remote_standard(
    res[1,],
    target,
    tree,
    conf,
    cache,
    on_progress = NULL
  ))
  expect_true(dl2 %in% c("Had", "Current"))
  expect_true(file.exists(target))
})

test_that("satisfy_remote", {

  res <- make_fake_resolution(`crayon@>=1.0.0` = list(type = "standard"))

  ## Package names differ
  bad1 <- make_fake_resolution(`crayon2` = list())
  expect_false(ans <- satisfy_remote_standard(res, bad1))
  expect_match(attr(ans, "reason"),  "Package names differ")

  ## Insufficient version
  bad2 <- make_fake_resolution(`crayon` = list(version = "0.0.1"))
  expect_false(ans <- satisfy_remote_standard(res, bad2))
  expect_match(attr(ans, "reason"),  "Insufficient version")

  ## Version is OK
  ok1 <- make_fake_resolution(`local::foobar` = list(package = "crayon"))
  expect_true(satisfy_remote_standard(res, ok1))

  ## No version req
  res <- make_fake_resolution(`crayon` = list(type = "standard"))
  ok2 <- make_fake_resolution(`local::foobar` = list(
    package = "crayon", version = "0.0.1"))
  expect_true(satisfy_remote_standard(res, ok2))

  ## Direct package must be from CRAN(like), must update
  res <- make_fake_resolution(`crayon` = list(
    type = "standard",
    package = "crayon",
    direct = TRUE,
    version = "1.1.1"
  ))

  bad3 <- make_fake_resolution(`installed::crayon` = list(
    type = "installed",
    package = "crayon",
    extra = list(list(repotype = NA_character_, remotetype = NA_character_))
  ))
  expect_false(satisfy_remote_standard(res, bad3))

  bad4 <- make_fake_resolution(`installed::crayon` = list(
    type = "installed",
    package = "crayon",
    extra = list(list())
  ))
  expect_false(satisfy_remote_standard(res, bad4))

  bad5 <- make_fake_resolution(`installed::crayon` = list(
    type = "installed",
    package = "crayon",
    extra = list(list(repotype = "cran", remotetype = "standard")),
    version = "1.0.0"
  ))
  expect_false(satisfy_remote_standard(res, bad5))

  bad6 <- make_fake_resolution(`installed::crayon` = list(
    type = "local",
    package = "crayon",
    extra = list(list(repotype = "cran", remotetype = "standard")),
    version = "1.0.0"
  ))
  expect_false(satisfy_remote_standard(res, bad6))

  ok3 <- make_fake_resolution(`installed::crayon` = list(
    type = "installed",
    package = "crayon",
    extra = list(list(repotype = "cran", remotetype = "standard")),
    version = "1.2.0"
  ))
  expect_true(satisfy_remote_standard(res, ok3))
})

test_that("installedok", {
  # CRAN binary
  sol <- list(
    repotype = "cran",
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

  # CRAN source
  sol <- list(
    repotype = "cran",
    package = "package",
    version = "1.0.0",
    platform = "source"
  )
  expect_true(installedok_remote_standard(
    list(
      package = "package",
      version = "1.0.0",
      repository = "CRAN"
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

  # Other binary
  sol <- list(
    repotype = NA_character_,
    package = "package",
    version = "1.0.0",
    platform = "*",
    built = "R 4.2.0; aarch64-apple-darwin20; 2022-10-08 08:37:45 UTC; unix",
    metadata = list(list(RemoteRepos = "foobar"))
  )
  expect_true(installedok_remote_standard(
    list(
      package = "package",
      version = "1.0.0",
      repotype = "bioc",
      remoterepos = "foobar",
      platform = "*",
      built = "R 4.2.0; aarch64-apple-darwin20; 2022-10-08 08:37:45 UTC; unix"
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

  # Other source
  sol <- list(
    repotype = NA_character_,
    package = "package",
    version = "1.0.0",
    platform = "source",
    metadata = list(list(RemoteRepos = "foobar"))
  )
  expect_true(installedok_remote_standard(
    list(
      package = "package",
      version = "1.0.0",
      repotype = "bioc",
      remoterepos = "foobar"
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
