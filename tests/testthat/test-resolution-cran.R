
context("resolution-cran")

test_that("explicit cran", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)
  do <- function(refs) {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes(refs), direct = TRUE)
    res$when_complete()
  }

  res <- synchronise(do("cran::dplyr"))
  expect_true(is_tibble(res))
  expect_true("cran::dplyr" %in% res$ref)
  expect_true(all(grep("::", res$ref, value = TRUE) == "cran::dplyr"))
  expect_equal(res$type, ifelse(res$ref == "cran::dplyr", "cran", "standard"))
  expect_equal(res$direct, res$ref == "cran::dplyr")
  expect_equal(res$status, rep("OK", nrow(res)))
  expect_equal(res$package, sub("^.*::", "", res$ref))
  expect_true(all(grepl(".", res$version, fixed = TRUE)))
  expect_true(all(res$platform == "source" | ! res$needscompilation))
  expect_true(all(is.na(res$built) | res$platform != "source"))
  expect_true(all(res$platform %in% default_platforms()))
  expect_true(all(res$rversion %in%
                  c(get_minor_r_version(current_r_version()), "*")))
  expect_true(is_character(res$repodir))
  expect_true(is_character(res$target))
  expect_true(all(vlapply(res$deps, is_tibble)))
  expect_true("imports" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true("suggests" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true(all(res$platform == "source" |
                  viapply(res$sources, length) == 1))
  expect_true(all(vlapply(res$remote, inherits, what = "remote_ref")))
  expect_true(all(vlapply(res$error, identical, list())))
  ## TODO: metadata
})

test_that("standard", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)
  do <- function(refs) {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes(refs), direct = TRUE)
    res$when_complete()
  }

  res <- synchronise(do("dplyr"))
  expect_true(is_tibble(res))
  expect_true("dplyr" %in% res$ref)
  expect_true(! any(grepl("::", res$ref)))
  expect_equal(res$type, ifelse(res$ref == "cran::dplyr", "cran", "standard"))
  expect_equal(res$direct, res$ref == "dplyr")
  expect_equal(res$status, rep("OK", nrow(res)))
  expect_equal(res$package, sub("^.*::", "", res$ref))
  expect_true(all(grepl(".", res$version, fixed = TRUE)))
  expect_true(all(res$platform == "source" | ! res$needscompilation))
  expect_true(all(is.na(res$built) | res$platform != "source"))
  expect_true(all(res$platform %in% default_platforms()))
  expect_true(all(res$rversion %in%
                  c(get_minor_r_version(current_r_version()), "*")))
  expect_true(is_character(res$repodir))
  expect_true(is_character(res$target))
  expect_true(all(vlapply(res$deps, is_tibble)))
  expect_true("imports" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true("suggests" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true(all(res$platform == "source" |
                  viapply(res$sources, length) == 1))
  expect_true(all(vlapply(res$remote, inherits, what = "remote_ref")))
  expect_true(all(vlapply(res$error, identical, list())))
  ## TODO: metadata
})

test_that("dependencies are honoured", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)
  do <- function(refs, deps) {
    conf$dependencies <- deps
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes(refs), direct = TRUE)
    res$when_complete()
  }

  ## FALSE means nothing
  res <- synchronise(do("cran::dplyr", FALSE))
  expect_true(all(res$direct))
  expect_true(all(res$ref == "cran::dplyr"))

  ## Exactly the specified ones
  res <- synchronise(do("cran::dplyr", "Imports"))
  imported <- unique(unlist(
    lapply(res$deps, function(x) x$package[x$type == "imports"])))
  expect_true(all(res$package[!res$direct] %in% imported))

  ## NA means hard ones
  res <- synchronise(do("cran::dplyr", NA))
  hard <- c("imports", "depends", "linkingto")
  harddep <- unique(unlist(
    lapply(res$deps, function(x) x$package[x$type %in% hard])))
  expect_true(all(res$package[!res$direct] %in% harddep))

  ## TRUE means hard + suggests on direct, hard only on rest
  res <- synchronise(do("cran::dplyr", TRUE))
  harddep <- unique(unlist(
    lapply(res$deps, function(x) x$package[x$type %in% hard])))
  softdirectdep <- unique(unlist(
    lapply(res$deps[res$direct], function(x) x$package[x$type == "suggests"])))
  indirect <- res$package[!res$direct]
  expect_true(all(indirect %in% harddep | indirect %in% softdirectdep))
})

test_that("error if cannot find package", {
  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = global_metadata_cache)
  do <- function(refs) {
    res <- resolution$new(config = conf, cache = cache)
    res$push(.list = parse_remotes(refs), direct = TRUE)
    res$when_complete()
  }

  bad <-  c("cran::thiscannotexistxxx", "neitherthisonexxx")
  res <- synchronise(do(bad))
  expect_equal(res$ref, bad)
  expect_equal(res$type, c("cran", "standard"))
  expect_equal(res$direct, c(TRUE, TRUE))
  expect_equal(res$status, c("FAILED", "FAILED"))
})

test_that("error if cannot find dependency", {
  ## TODO
})
