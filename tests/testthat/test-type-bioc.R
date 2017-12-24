
context("type bioc")

test_that("parse_remote", {

  pr <- parse_remotes("bioc::Biobase")[[1]]
  expect_equal(pr$package, "Biobase")
  expect_equal(pr$atleast, "")
  expect_equal(pr$version, "")
  expect_equal(pr$ref, "bioc::Biobase")
  expect_equal(pr$type, "bioc")
  expect_true("remote_ref_bioc" %in% class(pr))
  expect_true("remote_ref" %in% class(pr))
})

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "bioc::Biobase", config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$data$ref == "bioc::Biobase"))
  expect_true(all(res$data$type == "bioc"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
  expect_true(all(res$data$package == "Biobase"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  nonpkg <- paste0("bioc::", basename(tempfile()))
  r <- remotes$new(
    nonpkg, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))

  ## Existing package, non-existing version

  r <- remotes$new(
    "bioc::Biobase@0.0", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))
})

test_that("download_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "bioc::Biobase", config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      expect_error(r$resolve(), NA)
      expect_error(r$download_resolution(), NA)
    })
  dl <- r$get_resolution_download()

  expect_s3_class(dl, "remotes_downloads")
  expect_true(all(dl$data$ref == "bioc::Biobase"))
  expect_true(all(dl$data$type == "bioc"))
  expect_true(all(dl$data$direct))
  expect_true(all(dl$data$status == "OK"))
  expect_true(all(dl$data$package == "Biobase"))
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
    "bioc::Biobase", config = list(cache_dir = tmp), library = lib)
  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      expect_error(res <- r$resolve(), NA)
      expect_error(r$solve(), NA)
      expect_error(r$download_solution(), NA)
      expect_error(plan <- r$get_install_plan(), NA)
    })

  while (nrow(plan)) {
    to_install <- which(! viapply(plan$dependencies, length))
    if (!to_install) stop("Cannot install packages")
    for (w in to_install) {
      install.packages(plan$file[w], lib = lib, repos = NULL, quiet = TRUE,
                       type = "source")
    }
    pkgs <- plan$package[to_install]
    plan <- plan[ - to_install, drop = FALSE]
    plan$dependencies[] <- lapply(plan$dependencies, setdiff, y = pkgs)
  }

  withr::with_options(
    c(pkg.progress.bar = FALSE), {
      r$resolve()
      r$solve()
      plan <- r$get_install_plan()
    })

  expect_true(all(plan$type == "installed"))

  ver <- plan$version[match("Biobase", plan$package)]

  ref <- paste0("bioc::Biobase@>=", ver)
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
