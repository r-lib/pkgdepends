
context("type github")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  refs <- c(
    "r-lib/crayon",
    "github::r-lib/crayon",
    "crayon=r-lib/crayon",
    "crayon=github::r-lib/crayon",
    "wesm/feather/R",
    "r-lib/crayon@b5221ab0246050",
    "r-lib/crayon#61",
    "r-lib/testthat@*release"
  )

  r <- remotes$new(
    refs, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_equal(sort(res$data$ref), sort(refs))
  expect_true(all(res$data$type == "github"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  nonrepo <- paste0(basename(tempfile()), "/", basename(tempfile()))
  r <- remotes$new(
    nonrepo, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))

  ## Existing repo, no R package there

  r <- remotes$new(
    "github::r-lib/crayon/R", config = list(cache_dir = tmp))
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
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ref <- "github::r-lib/crayon@b5221ab0246050dc687dc8b9964d5c44c947b265"
  r <- remotes$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE), {
      expect_error(r$resolve(), NA)
      expect_error(r$download_resolution(), NA)
    })
  dl <- r$get_resolution_download()

  expect_s3_class(dl, "remotes_downloads")
  expect_true(dl$data$ref == ref)
  expect_true(dl$data$type == "github")
  expect_true(dl$data$direct)
  expect_true(dl$data$status == "OK")
  expect_true(dl$data$package == "crayon")
  expect_true(dl$data$download_status %in% c("Got", "Had"))
})

test_that("satisfies_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  ref <- "github::r-lib/crayon@b5221ab0246050dc687dc8b9964d5c44c947b265"
  r <- remotes$new(
    ref, config = list(cache_dir = tmp), library = lib)
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

  dsc <- desc::desc(file.path(lib, "crayon"))
  dsc$set("RemoteSha", parse_remotes(ref)[[1]]$commitish)
  dsc$write()

  withr::with_options(
    c(pkg.show_progress = FALSE), {
      r$resolve()
      r$solve()
      plan <- r$get_install_plan()
    })

  expect_true(all(plan$type == "installed"))

  r <- remotes$new(ref, config = list(cache_dir = tmp), library = lib)
  withr::with_options(
    c(pkg.show_progress = FALSE), {
      expect_error(res <- r$resolve(), NA)
      expect_error(r$solve(), NA)
      expect_error(plan <- r$get_install_plan(), NA)
    })

  expect_true(all(plan$type == "installed"))
})
