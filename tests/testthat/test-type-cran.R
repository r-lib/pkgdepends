
context("type cran")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon", config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$data$ref == "cran::crayon"))
  expect_true(all(res$data$type == "cran"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
  expect_true(all(res$data$package == "crayon"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  nonpkg <- paste0("cran::", basename(tempfile()))
  r <- remotes$new(
    nonpkg, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))

  ## Existing package, non-existing version

  r <- remotes$new(
    "cran::crayon@0.0", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))
})

test_that("resolve current version", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    c("cran::crayon", "cran::crayon@current"),
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$data$type == "cran"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
  expect_true(all(res$data$package == "crayon"))

  cur <- which(res$data$ref == "cran::crayon@current")
  for (w in cur) {
    expect_true(res$data$version[w] %in% res$data$version[-cur])
  }
})

test_that("resolve an old version", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon@1.1.0",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$data$ref == "cran::crayon@1.1.0"))
  expect_true(all(res$data$type == "cran"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
  expect_true(all(res$data$package == "crayon"))
  expect_true(all(res$data$version == "1.1.0"))
})

test_that("resolve current version, specified via version number", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon@current",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  ver <- res$data$version[1]

  ref <- paste0("cran::crayon@", ver)
  r2 <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r2$resolve(), NA))
  res2 <- r2$get_resolution()

  expect_true(all(res2$data$version == ver))
  expect_true(all(res2$data$status == "OK"))
})

test_that("resolve a version range", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon@>=1.3.2",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$data$ref == "cran::crayon@>=1.3.2"))
  expect_true(all(res$data$type == "cran"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
  expect_true(all(res$data$package == "crayon"))
  expect_true(all(package_version(res$data$version) >= "1.3.2"))
})

test_that("download_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon", config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      expect_error(r$resolve(), NA)
      expect_error(r$download_resolution(), NA)
    })
  dl <- r$get_resolution_download()

  expect_true(all(file.exists(dl$data$fulltarget)))
  expect_s3_class(dl, "remotes_downloads")
  expect_true(all(dl$data$ref == "cran::crayon"))
  expect_true(all(dl$data$type == "cran"))
  expect_true(all(dl$data$direct))
  expect_true(all(dl$data$status == "OK"))
  expect_true(all(dl$data$package == "crayon"))
  expect_true(all(dl$download_status == "Got"))
})

test_that("satisfies_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon", config = list(cache_dir = tmp), library = lib)
  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      expect_error(res <- r$resolve(), NA)
      expect_error(r$solve(), NA)
      expect_error(r$download_solution(), NA)
      expect_error(plan <- r$get_install_plan(), NA)
    })

  while (nrow(plan)) {
    to_install <- which(! viapply(plan$dependencies, length))
    if (!length(to_install)) stop("Cannot install packages")
    for (w in to_install) {
      install.packages(plan$file[w], lib = lib, repos = NULL, quiet = TRUE,
                       type = "source")
    }
    pkgs <- plan$package[to_install]
    plan <- plan[ - to_install, ]
    plan$dependencies[] <- lapply(plan$dependencies, setdiff, y = pkgs)
  }

  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      r$resolve()
      r$solve()
      plan <- r$get_install_plan()
    })

  expect_true(all(plan$type == "installed"))

  ver <- plan$version[match("crayon", plan$package)]

  ref <- paste0("cran::crayon@>=", ver)
  r <- remotes$new(ref, config = list(cache_dir = tmp), library = lib)
  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      expect_error(res <- r$resolve(), NA)
      expect_error(r$solve(), NA)
      expect_error(r$download_solution(), NA)
      expect_error(plan <- r$get_install_plan(), NA)
    })

  expect_true(all(plan$type == "installed"))
})
