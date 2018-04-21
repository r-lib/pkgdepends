
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

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)

  res <- synchronise(
    resolve_remote_bioc(parse_remotes("bioc::Biobase")[[1]], TRUE, conf,
                        cache, dependencies = FALSE)
  )

  expect_true(is_tibble(res))
  expect_true(all(res$ref == "bioc::Biobase"))
  expect_true(all(res$type == "bioc"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "Biobase"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)

  ref <- paste0("bioc::", basename(tempfile()))
  res <- synchronise(
    resolve_remote_bioc(parse_remotes(ref)[[1]], TRUE, conf,
                        cache, dependencies = FALSE)
  )

  expect_true(all(res$status == "FAILED"))

  ## Existing package, non-existing version

  skip("TODO")

  r <- remotes$new(
    "bioc::Biobase@0.0", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))
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

  res <- synchronise(
    resolve_remote_bioc(parse_remotes("bioc::Biobase")[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  target <- file.path(conf$cache_dir, res$target[1])
  dl <- synchronise(
    download_remote_bioc(res[1,], target, conf, cache, progress_bar = NULL))

  expect_equal(dl, "Got")
  expect_true(file.exists(target))

  unlink(target)
  dl2 <- synchronise(
    download_remote_bioc(res[1,], target, conf, cache, progress_bar = NULL))
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
    "bioc::Biobase", config = list(cache_dir = tmp), library = lib)
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

  ver <- plan$version[match("Biobase", plan$package)]

  ref <- paste0("bioc::Biobase@>=", ver)
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
