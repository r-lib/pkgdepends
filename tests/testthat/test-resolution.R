
context("resolution")

test_that("dependencies config parameter is honored", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  do <- function(deps) {
    r <- remotes$new(
      "dplyr", config = list(dependencies = deps, cache_dir = tmp))
    withr::with_options(
      c(pkg.progress.bar = FALSE),
      expect_error(r$resolve(), NA))
    r$get_resolution()
  }

  ## Exactly the specified ones
  d <- do("Imports")$data
  expect_true(all(unlist(lapply(d$dependencies, "[[", "type")) ==
                  "Imports"))

  hard <- c("Imports", "Depends", "LinkingTo")

  ## NA (this is also the default), hard dependencies only
  d <- do(NA)$data
  expect_true(all(unlist(lapply(d$dependencies, "[[", "type")) %in% hard))

  ## TRUE means hard + Suggests on direct ones, hard only on rest
  d <- do(TRUE)$data
  direct <- unlist(lapply(d$dependencies[d$direct], "[[", "type"))
  expect_true(all(direct %in% c(hard, "Suggests")))
  expect_true("Imports" %in% direct)
  expect_true("Depends" %in% direct)
  expect_true("LinkingTo" %in% direct)
  expect_true("Suggests" %in% direct)

  indirect <- unlist(lapply(d$dependencies[!d$direct], "[[", "type"))
  expect_true(all(indirect %in% hard))
  expect_true("Imports" %in% indirect)
  expect_true("Depends" %in% indirect)
  expect_true("LinkingTo" %in% indirect)
  expect_false("Suggests" %in% indirect)

  ## FALSE means nothing
  d <- do(FALSE)$data
  expect_true(all(d$ref == "dplyr"))
})

test_that("resolving installed packages", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  withr::with_options(c(pkg.progress.bar = FALSE), {
    r <- remotes$new("crayon", config = list(dependencies = FALSE),
                     library = tmp)
    r$solve()
    r$download_solution()
    pkg_file<- r$get_install_plan()$file
    install.packages(pkg_file, lib = tmp, repos = NULL, type = "source",
                     quiet = TRUE)

    r2 <- remotes$new("crayon", config = list(dependencies = FALSE),
                      library = tmp)
    r2$solve()
    r2$download_solution()
    plan <- r2$get_install_plan()
  })

  expect_equal(plan$type, "installed")
  expect_identical(plan$file, NA_character_)
  expect_equal(plan$installed, file.path(tmp, "crayon"))
})

test_that("print", {
  res <- read_fixture("resolution-installed.rds")
  expect_output(
    print(res),
    "RESOLUTION.*installed.*pkgconfig"
  )
})

test_that("print, with errors", {

  skip_if_offline()
  skip_on_cran()

  withr::with_options(c(pkg.progress.bar = FALSE), {
    npkg1 <- basename(tempfile())
    npkg2 <- basename(tempfile())
    r <- remotes$new(c(npkg1, paste0("r-lib/", npkg2)), lib = tempfile())
    res <- r$resolve()
  })

  expect_output(print(res), "Errors:")
  expect_output(print(res), "Can't find CRAN/BioC package")
  expect_output(print(res), "r-lib/.*: Unknown error")
})
