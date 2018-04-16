
context("resolution")

test_that("resolving installed packages", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  withr::with_options(c(pkg.show_progress = FALSE), {
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

  withr::with_options(c(pkg.show_progress = FALSE), {
    npkg1 <- basename(tempfile())
    npkg2 <- basename(tempfile())
    r <- remotes$new(c(npkg1, paste0("r-lib/", npkg2)), lib = tempfile())
    res <- r$resolve()
  })

  expect_output(print(res), "Errors:")
  expect_output(print(res), "Can't find CRAN/BioC package")
  expect_output(print(res), "r-lib/.*: Not Found")
})
