
context("type cran")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)

  res <- synchronise(
    resolve_remote_cran(parse_remotes("cran::crayon")[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(is_tibble(res))
  expect_true(all(res$ref == "cran::crayon"))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)

  nonpkg <- paste0("cran::", basename(tempfile()))
  res <- synchronise(
    resolve_remote_cran(parse_remotes(nonpkg)[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(all(res$status == "FAILED"))
  expect_equal(conditionMessage(res$error[[1]]),
               "Cannot find standard package")

  ## Existing package, non-existing version

  skip("TODO")

  r <- remotes$new(
    "cran::crayon@0.0", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))
})

test_that("resolve current version", {
  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)

  do <- function(ref) {
    resolve_remote_cran(parse_remotes(ref)[[1]], TRUE,
                        conf, cache, dependencies = FALSE)
  }

  res <- synchronise(do("cran::crayon@current"))
  res2 <- synchronise(do("cran::crayon"))

  expect_true(is_tibble(res))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))

  expect_equal(res$version, res2$version)
})

test_that("resolve an old version", {
  skip_if_offline()
  skip_on_cran()
  skip("TODO")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon@1.1.0",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
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
  skip("TODO")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon@current",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  ver <- res$version[1]

  ref <- paste0("cran::crayon@", ver)
  r2 <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r2$resolve(), NA))
  res2 <- r2$get_resolution()

  expect_true(all(res2$version == ver))
  expect_true(all(res2$status == "OK"))
})

test_that("resolve a version range", {
  skip_if_offline()
  skip_on_cran()
  skip("TODO")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes$new(
    "cran::crayon@>=1.3.2",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$ref == "cran::crayon@>=1.3.2"))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))
  expect_true(all(package_version(res$data$version) >= "1.3.2"))
})

test_that("download_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  conf <- remotes_default_config()
  conf$platforms <- "macos"
  conf$cache_dir <- tmp
  conf$package_cache_dir <- tmp2
  cache <- list(
    package = package_cache$new(conf$package_cache_dir),
    metadata = global_metadata_cache)

  resolve <- function() {
    resolve_remote_cran(parse_remotes("cran::crayon")[[1]], TRUE, conf, cache,
                        dependencies = FALSE)
  }
  res <- synchronise(resolve())

  target <- file.path(conf$cache_dir, res$target[1])
  download <- function(res) {
    download_remote_cran(res, target, conf, cache, progress_bar = NULL)
  }
  dl1 <- synchronise(download(res[1,]))
  expect_equal(dl1, "Got")
  expect_true(file.exists(target))

  unlink(target)
  dl2 <- synchronise(download(res[1,]))
  expect_equal(dl2, "Current")
  expect_true(file.exists(target))
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
    c(pkg.show_progress = FALSE), {
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
    c(pkg.show_progress = FALSE), {
      r$resolve()
      r$solve()
      plan <- r$get_install_plan()
    })

  expect_true(all(plan$type == "installed"))

  ver <- plan$version[match("crayon", plan$package)]

  ref <- paste0("cran::crayon@>=", ver)
  r <- remotes$new(ref, config = list(cache_dir = tmp), library = lib)
  withr::with_options(
    c(pkg.show_progress = FALSE), {
      expect_error(res <- r$resolve(), NA)
      expect_error(r$solve(), NA)
      expect_error(r$download_solution(), NA)
      expect_error(plan <- r$get_install_plan(), NA)
    })

  expect_true(all(plan$type == "installed"))
})
