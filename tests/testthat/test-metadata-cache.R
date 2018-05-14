
context("metadata cache")

test_that("get_cache_files", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  check <- function(files, root) {
    expect_equal(files$root, root)
    expect_true(all(c("meta", "lock", "rds") %in% names(files)))
    expect_equal(
      fs::path_common(c(files$rds, files$lock, files$rds, root)),
      root)
    expect_true(tibble::is_tibble(files$pkgs))
    expect_equal(
      sort(names(files$pkgs)),
      sort(c("path", "etag", "basedir", "base", "mirror", "url",
             "platform", "type", "bioc_version")))
    expect_equal(
      fs::path_common(c(files$pkgs$path, files$pkgs$etag, root)),
      root)
  }

  check(pri_files, pri)
  check(rep_files, rep)
})

test_that("get_current_data", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  set_private(cmc, "data") <- "DATA"
  set_private(cmc, "data_time") <- Sys.time()
  expect_equal(get_private(cmc)$get_current_data(oneday()), "DATA")

  set_private(cmc,  "data_time") <- Sys.time() - 2 * oneday()
  expect_error(
    get_private(cmc)$get_current_data(oneday()),
    "Loaded data outdated")

  set_private(cmc, "data_time") <- NULL
  expect_error(
    get_private(cmc)$get_current_data(oneday()),
    "Loaded data outdated")

  set_private(cmc, "data") <- NULL
  expect_error(get_private(cmc)$get_current_data(oneday()), "No data loaded")
})

test_that("load_replica_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  expect_error(
    get_private(cmc)$load_replica_rds(oneday()),
    "No replica RDS file in cache"
  )

  rep_files <- get_private(cmc)$get_cache_files("replica")
  mkdirp(dirname(rep_files$rds))
  saveRDS("This is it", rep_files$rds)
  file_set_time(rep_files$rds, Sys.time() - 2 * oneday())
  expect_error(
    get_private(cmc)$load_replica_rds(oneday()),
    "Replica RDS cache file outdated"
  )

  file_set_time(rep_files$rds, Sys.time() - 1/2  * oneday())
  expect_equal(
    get_private(cmc)$load_replica_rds(oneday()),
    "This is it")
  expect_equal(get_private(cmc)$data, "This is it")
  expect_true(Sys.time() - get_private(cmc)$data_time < oneday())
})

test_that("load_primary_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source",  bioc = FALSE)

  expect_error(
    get_private(cmc)$load_primary_rds(oneday()),
    "No primary RDS file in cache"
  )

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$rds))
  saveRDS("This is it", pri_files$rds)
  file_set_time(pri_files$rds, Sys.time() - 2 * oneday())
  expect_error(
    get_private(cmc)$load_primary_rds(oneday()),
    "Primary RDS cache file outdated"
  )

  file_set_time(pri_files$rds, Sys.time() - 1/2  * oneday())
  expect_equal(
    get_private(cmc)$load_primary_rds(oneday()),
    "This is it")
  expect_equal(get_private(cmc)$data, "This is it")
  expect_true(Sys.time() - get_private(cmc)$data_time < oneday())

  ## Replica was also updated
  expect_equal(
    get_private(cmc)$load_replica_rds(oneday()),
    "This is it")
})

test_that("load_primary_pkgs", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  expect_error(
    get_private(cmc)$load_primary_pkgs(oneday()),
    "Some primary PACKAGES files don't exist")

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-mac.gz"), pri_files$pkgs$path[1])
  expect_error(
    synchronise(get_private(cmc)$load_primary_pkgs(oneday())),
    "Some primary PACKAGES files don't exist")

  for (i in tail(seq_len(nrow(pri_files$pkgs)), -1)) {
    fs::file_copy(get_fixture("PACKAGES-src.gz"), pri_files$pkgs$path[i])
  }
  file_set_time(pri_files$pkgs$path, Sys.time() - 2 * oneday())
  expect_error(
    synchronise(get_private(cmc)$load_primary_pkgs(oneday())),
    "Some primary PACKAGES files are outdated")

  file_set_time(pri_files$pkgs$path, Sys.time() - 1/2 * oneday())
  res <- synchronise(get_private(cmc)$load_primary_pkgs(oneday()))
  check_packages_data(res)

  ## RDS was updated as well
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneday())

  ## Primary RDS was updated as well
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())
})

test_that("update_replica_pkgs", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  synchronise(get_private(cmc)$update_replica_pkgs())
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(all(file.exists(rep_files$pkgs$path)))
  expect_true(all(file.exists(rep_files$pkgs$etag)))

  data <- get_private(cmc)$update_replica_rds()
  expect_identical(data, get_private(cmc)$data)
  check_packages_data(data)
})

test_that("update_replica_rds", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  rep_files <- get_private(cmc)$get_cache_files("replica")
  mkdirp(dirname(rep_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-mac.gz"), rep_files$pkgs$path[1])
  for (i in tail(seq_len(nrow(rep_files$pkgs)), -1)) {
    fs::file_copy(get_fixture("PACKAGES-src.gz"), rep_files$pkgs$path[i])
  }

  data <- get_private(cmc)$update_replica_rds()
  expect_identical(get_private(cmc)$data, data)
  expect_true(get_private(cmc)$data_time > Sys.time() - oneminute())
  check_packages_data(data)
})

test_that("update_primary", {
  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, c("macos", "source"),
                                     bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  rep_files <- get_private(cmc)$get_cache_files("replica")

  mkdirp(dirname(rep_files$rds))
  saveRDS("RDS", rep_files$rds)
  get_private(cmc)$update_primary(rds = TRUE, packages = FALSE)
  expect_true(file.exists(pri_files$rds))
  expect_equal(readRDS(pri_files$rds), "RDS")

  lapply_rows(rep_files$pkgs, function(pkg) {
    mkdirp(dirname(pkg$path))
    cat(basename(pkg$path), "\n", sep = "", file = pkg$path)
    mkdirp(dirname(pkg$etag))
    cat(pkg$url, "\n", sep = "", file = pkg$etag)
  })
  get_private(cmc)$update_primary(rds = FALSE, packages = TRUE)
  expect_true(all(file.exists(pri_files$pkgs$path)))
  expect_true(all(file.exists(pri_files$pkgs$etag)))

  lapply_rows(pri_files$pkgs, function(pkg) {
    expect_equal(readLines(pkg$path), basename(pkg$path))
    expect_equal(readLines(pkg$etag), pkg$url)
  })
})

test_that("update", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)
  data <- cmc$update()
  check_packages_data(data)

  ## Data is loaded
  expect_identical(get_private(cmc)$data, data)
  expect_true(Sys.time() - get_private(cmc)$data_time < oneminute())

  ## There is a replica RDS
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneminute())

  ## There is a primary RDS
  pri_files <- get_private(cmc)$get_cache_files("primary")
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())

  ## There are replicate PACKAGES, with Etag files
  expect_true(all(file.exists(rep_files$pkgs$path)))
  expect_true(all(file.exists(rep_files$pkgs$etag)))

  ## There are primary PACKAGES, with Etag files
  expect_true(all(file.exists(pri_files$pkgs$path)))
  expect_true(all(file.exists(pri_files$pkgs$etag)))
})

test_that("deps will auto-update as needed", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-src.gz"), pri_files$pkgs$path)

  ## This will update the RDS files, and also load the data
  cmc$deps("A3", recursive = FALSE)

  ## Data is loaded
  expect_false(is.null(get_private(cmc)$data))
  expect_true(Sys.time() - get_private(cmc)$data_time < oneminute())

  ## There is a replica RDS
  rep_files <- get_private(cmc)$get_cache_files("replica")
  expect_true(file.exists(rep_files$rds))
  expect_true(Sys.time() - file_get_time(rep_files$rds) < oneminute())

  ## There is a primary RDS
  pri_files <- get_private(cmc)$get_cache_files("primary")
  expect_true(file.exists(pri_files$rds))
  expect_true(Sys.time() - file_get_time(pri_files$rds) < oneminute())

  ## There are replicate PACKAGES, no Etag files, since no downloads...
  expect_true(all(file.exists(rep_files$pkgs$path)))

  ## There are primary PACKAGES, no Etag files, since no downloads...
  expect_true(all(file.exists(pri_files$pkgs$path)))
})

test_that("deps, extract_deps", {

  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE,
                                     cran_mirror = "mirror")

  pri_files <- get_private(cmc)$get_cache_files("primary")
  mkdirp(dirname(pri_files$pkgs$path))
  fs::file_copy(get_fixture("PACKAGES-src.gz"), pri_files$pkgs$path)
  file_set_time(pri_files$pkgs$path, Sys.time() - 1/2 * oneday())

  pkgs <- read_packages_file(
    get_fixture("PACKAGES-src.gz"),
    mirror = "mirror", repodir = "src/contrib", platform = "source",
    rversion = get_minor_r_version(current_r_version()), type = "cran")

  deps <- cmc$deps("abc", FALSE, FALSE)
  expect_identical(deps$package, "abc")
  expect_identical(attr(deps, "base"), character())
  expect_identical(attr(deps, "unknown"), character())
  deps2 <- extract_deps(pkgs, "abc", FALSE, FALSE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "abc", TRUE, FALSE)
  expect_identical(deps$package, c("abc", "abc.data", "MASS", "nnet"))
  expect_identical(attr(deps, "base"), character())
  expect_identical(attr(deps, "unknown"), c("quantreg", "locfit"))
  deps2 <- extract_deps(pkgs, "abc", TRUE, FALSE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "abc", TRUE, TRUE)
  expect_identical(deps$package, c("abc", "abc.data", "MASS", "nnet"))
  expect_identical(
    sort(attr(deps, "base")),
    sort(c("grDevices", "graphics", "stats", "utils", "methods")))
  expect_identical(attr(deps, "unknown"), c("quantreg", "locfit"))
  deps2 <- extract_deps(pkgs, "abc", TRUE, TRUE)
  expect_identical(deps, deps2)

  deps <- extract_deps(pkgs, "nnet", c("Depends", "Suggests"), FALSE)
  expect_identical(deps$package, c("MASS", "nnet"))
  expect_identical(attr(deps, "base"), c("stats", "utils"))
  expect_identical(attr(deps, "unknown"), character())
  deps2 <- extract_deps(pkgs, "nnet", c("Depends", "Suggests"), FALSE)
  expect_identical(deps, deps2)
})

test_that("concurrency in update", {
  skip_if_offline()

  dir.create(pri <- fs::path_norm(tempfile()))
  on.exit(unlink(pri, recursive = TRUE), add = TRUE)
  dir.create(rep <- fs::path_norm(tempfile()))
  on.exit(unlink(rep, recursive = TRUE), add = TRUE)

  cmc <- cranlike_metadata_cache$new(pri, rep, "source", bioc = FALSE)

  ## TODO: somehow check that there are no parallel downloads
  do <- function() {
    dx1 <- cmc$async_update()
    dx2 <- cmc$async_update()
    dx3 <- cmc$async_update()
    when_all(dx1, dx2, dx3)
  }

  res <- synchronise(do())
  check_packages_data(res[[1]])
  check_packages_data(res[[2]])
  check_packages_data(res[[3]])

  expect_null(get_private(cmc)$update_deferred)
})

test_that("cmc__get_repos", {
  repos <- c(CRAN = "bad")

  ## No bioc, CRAN is replaced
  expect_equal(
    cmc__get_repos(repos, FALSE, cran_mirror = "good", r_version = "3.5"),
    tibble(name = "CRAN", url = "good", type = "cran",
           bioc_version = NA_character_)
  )

  ## BioC, all new
  res <- cmc__get_repos(repos, TRUE, "good", r_version = "3.5")
  expect_equal(res$name, c("CRAN", "BioCsoft", "BioCann", "BioCexp"))
  expect_equal(res$url[1], "good")
  expect_equal(res$type, c("cran", "bioc", "bioc", "bioc"))
  expect_equal(res$bioc_version, c(NA_character_, "3.7", "3.7", "3.7"))

  ## BioC, some are custom
  repos <- c(CRAN = "bad", BioCsoft = "ok")
  res <- cmc__get_repos(repos, TRUE, "good", r_version = "3.5")
  expect_equal(res$name, c("CRAN", "BioCsoft", "BioCann", "BioCexp"))
  expect_equal(res$url[1], "good")
  expect_equal(res$url[2], "ok")
  expect_equal(res$type, c("cran", "bioc", "bioc", "bioc"))
  expect_equal(res$bioc_version, c(NA_character_, "3.7", "3.7", "3.7"))
})
