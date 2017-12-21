
context("resolution")

test_that("dependencies config parameter is honored", {

  skip_if_offline()

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
