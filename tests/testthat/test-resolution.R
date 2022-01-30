
test_that("resolving with a list", {
  conf <- current_config()
  cache <- list(package = pkgcache::package_cache$new(), metadata = NULL)

  do <- function() {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs("foo::bar"))
    res$push(.list = parse_pkg_refs("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$rest,
         version = "1.0.0", sources = c("src1", "src2"), direct = direct)
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 2)
  expect_identical(res$ref, c("foo::bar", "foo::bar2"))
  expect_identical(res$package, c("bar", "bar2"))
  expect_identical(res$status, c("OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(res$sources, list(c("src1", "src2"), c("src1", "src2")))
})

test_that("resolving with a tbl", {
  conf <- current_config()
  cache <- list(package = pkgcache::package_cache$new(), metadata = NULL)
  do <- function() {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs("foo::bar"))
    res$push(.list = parse_pkg_refs("foo2::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    data_frame(ref = remote$ref, type = c("type1", "type2"),
           package = c("pkg1", "pkg2"), version = c("ver1", "ver2"),
           sources = list(c("s11", "s12"), c("s21", "s22")),
           direct = direct)
  }

  types <- list(foo = list(resolve = foo_resolve),
                foo2 = list(resolve = foo_resolve))
  res <- withr::with_options(
    list(pkg.remote_types = types),
    synchronise(do()))

  expect_equal(nrow(res), 4)
  expect_identical(res$ref, c("foo::bar", "foo::bar", "foo2::bar2", "foo2::bar2"))
  expect_identical(res$package, c("pkg1", "pkg2", "pkg1", "pkg2"))
  expect_identical(res$status, c("OK", "OK", "OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(
    res$sources,
    list(c("s11", "s12"), c("s21", "s22"), c("s11", "s12"), c("s21", "s22"))
  )
})

test_that("unknown deps are pushed in the queue", {
  conf <- current_config()
  cache <- list(package = pkgcache::package_cache$new(), metadata = NULL)
  do <- function() {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs("foo::bar"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$rest,
         version = "1.0.0", sources = c("src1", "src2"),
         unknown_deps = "foo::bar2", direct = direct)
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 2)
  expect_identical(res$ref, c("foo::bar", "foo::bar2"))
  expect_identical(res$package, c("bar", "bar2"))
  expect_identical(res$status, c("OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(res$sources, list(c("src1", "src2"), c("src1", "src2")))
})

test_that("unknown deps, tble", {
  conf <- current_config()
  cache <- list(package = pkgcache::package_cache$new(), metadata = NULL)
  do <- function() {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs("foo::bar"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    data_frame(ref = remote$ref, type = c("type1", "type2"),
           package = c("pkg1", "pkg2"), version = c("ver1", "ver2"),
           sources = list(c("s11", "s12"), c("s21", "s22")),
           unknown_deps = "foo::bar2", direct = direct,
           params = list(character()))
  }

  types <- list(foo = list(resolve = foo_resolve),
                foo2 = list(resolve = foo_resolve))
  res <- withr::with_options(
    list(pkg.remote_types = types),
    synchronise(do()))

  expect_equal(nrow(res), 4)
  expect_identical(res$ref, c("foo::bar", "foo::bar", "foo::bar2", "foo::bar2"))
  expect_identical(res$package, c("pkg1", "pkg2", "pkg1", "pkg2"))
  expect_identical(res$status, c("OK", "OK", "OK", "OK"))
  expect_true(is.list(res$remote[[1]]))
  expect_true(is.list(res$remote[[2]]))
  expect_identical(
    res$sources,
    list(c("s11", "s12"), c("s21", "s22"), c("s11", "s12"), c("s21", "s22"))
  )
})

test_that("error", {
  conf <- current_config()
  cache <- list(package = pkgcache::package_cache$new(), metadata = NULL)
  do <- function() {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs("foo::bar"))
    res$push(.list = parse_pkg_refs("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    stop("foobar", call. = FALSE)
  }

  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(resolve = foo_resolve))),
    synchronise(do()))

  expect_equal(nrow(res), 2)
  expect_equal(res$ref, c("foo::bar", "foo::bar2"))
  expect_s3_class(res$error[[1]], "error")
  expect_s3_class(res$error[[2]], "error")
  expect_equal(conditionMessage(res$error[[1]]), "foobar")
  expect_equal(conditionMessage(res$error[[2]]), "foobar")
  expect_equal(res$status, c("FAILED", "FAILED"))
  expect_equal(res$type, c("foo", "foo"))
})

test_that("installed refs are also resolved", {
  conf <- current_config()
  mkdirp(lib <- tempfile())
  lib <- normalizePath(lib)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  mkdirp(file.path(lib, "bar"))
  mkdirp(file.path(lib, "bar2"))
  cache <- list(package = pkgcache::package_cache$new(), metadata = NULL)
  cache$installed  <- list(
    pkgs = data_frame(
      ref = paste0("installed::", lib, c("/bar", "/bar2")),
      type = "installed",
      status = "OK",
      package = c("bar", "bar2"),
      version = "1.0.9", license = NA_character_, needscompilation = TRUE,
      priority = NA_character_, md5sum = NA_character_,
      platform = "source", rversion = "*", sources = list(NULL, NULL),
      built = NA_character_, deps = list(NULL, NULL),
      repotype = NA_character_,
      sysreqs = NA_character_)
  )

  do <- function() {
    res <- new_resolution(config = conf, cache = cache, library = lib)
    res$push(.list = parse_pkg_refs("foo::bar"))
    res$push(.list = parse_pkg_refs("foo::bar2"))
    res$when_complete()
  }

  foo_resolve <- function(remote, direct, config, cache, dependencies) {
    list(ref = remote$ref, type = remote$type, package = remote$rest,
         version = "1.0.0", sources = c("src1", "src2"), direct = direct)
  }

  types <- list(foo = list(resolve = foo_resolve))

  res <- withr::with_options(
    list(pkg.remote_types = types),
    synchronise(do()))

  expect_equal(nrow(res), 4)
  expect_equal(res$ref[1:2], c("foo::bar", "foo::bar2"))
  expect_equal(res$ref[3:4], paste0("installed::", lib, "/", c("bar", "bar2")))
  expect_equal(res$status, rep("OK", 4))
  expect_equal(res$package, c("bar", "bar2", "bar", "bar2"))
})

test_that("explicit cran", {
  skip_on_cran()
  conf <- current_config()
  cache <- list(package = pkgcache::package_cache$new(),
                metadata = pkgcache::get_cranlike_metadata_cache())
  do <- function(refs) {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs(refs), direct = TRUE)
    res$when_complete()
  }

  res <- synchronise(do("cran::dplyr"))
  expect_true(inherits(res, "tbl"))
  expect_true("cran::dplyr" %in% res$ref)
  expect_true(all(grep("::", res$ref, value = TRUE) == "cran::dplyr"))
  expect_equal(res$type, ifelse(res$ref == "cran::dplyr", "cran", "standard"))
  expect_equal(res$direct, res$ref == "cran::dplyr")
  expect_equal(res$status, rep("OK", nrow(res)))
  expect_equal(res$package, sub("^.*::", "", res$ref))
  expect_true(all(grepl(".", res$version, fixed = TRUE)))
  expect_true(all(res$platform == "source" | ! res$needscompilation))
  expect_true(all(is.na(res$built) | res$platform != "source"))
  good_platforms <- c(
    default_platforms(),
    if (is_windows()) "i386+x86_64-w64-mingw32"
  )
  expect_true(all(res$platform %in% good_platforms))
  expect_true(all(res$rversion %in%
                  c(get_minor_r_version(current_r_version()), "*")))
  expect_true(is_character(res$repodir))
  expect_true(is_character(res$target))
  expect_true(all(vlapply(res$deps, inherits, what = "tbl")))
  expect_true("imports" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true("suggests" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true(all(grepl("source|darwin", res$platform) |
                  viapply(res$sources, length) == 1))
  expect_true(all(vlapply(res$remote, inherits, what = "remote_ref")))
  expect_true(all(vlapply(res$error, identical, list())))
  ## TODO: metadata
})

test_that("standard", {
  skip_on_cran()
  conf <- current_config()
  cache <- list(
    package = pkgcache::package_cache$new(),
    metadata = pkgcache::get_cranlike_metadata_cache())
  do <- function(refs) {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs(refs), direct = TRUE)
    res$when_complete()
  }

  res <- synchronise(do("dplyr"))
  expect_true(inherits(res, "tbl"))
  expect_true("dplyr" %in% res$ref)
  expect_true(! any(grepl("::", res$ref)))
  expect_equal(res$type, ifelse(res$ref == "cran::dplyr", "cran", "standard"))
  expect_equal(res$direct, res$ref == "dplyr")
  expect_equal(res$status, rep("OK", nrow(res)))
  expect_equal(res$package, sub("^.*::", "", res$ref))
  expect_true(all(grepl(".", res$version, fixed = TRUE)))
  expect_true(all(res$platform == "source" | ! res$needscompilation))
  expect_true(all(is.na(res$built) | res$platform != "source"))
  good_platforms <- c(
    default_platforms(),
    if (is_windows()) "i386+x86_64-w64-mingw32"
  )
  expect_true(all(res$platform %in% good_platforms))
  expect_true(all(res$rversion %in%
                  c(get_minor_r_version(current_r_version()), "*")))
  expect_true(is_character(res$repodir))
  expect_true(is_character(res$target))
  expect_true(all(vlapply(res$deps, inherits, what = "tbl")))
  expect_true("imports" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true("suggests" %in% unlist(lapply(res$deps, "[[", "type")))
  expect_true(all(grepl("source|darwin", res$platform) |
                  viapply(res$sources, length) == 1))
  expect_true(all(vlapply(res$remote, inherits, what = "remote_ref")))
  expect_true(all(vlapply(res$error, identical, list())))
  ## TODO: metadata
})

test_that("dependencies are honoured", {
  conf <- current_config()
  cache <- list(
    package = pkgcache::package_cache$new(),
    metadata = pkgcache::get_cranlike_metadata_cache())
  do <- function(refs, deps) {
    conf$set("dependencies", deps)
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs(refs), direct = TRUE)
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
  conf <- current_config()
  cache <- list(
    package = pkgcache::package_cache$new(),
    metadata = pkgcache::get_cranlike_metadata_cache())
  do <- function(refs) {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs(refs), direct = TRUE)
    res$when_complete()
  }

  bad <-  c("cran::thiscannotexistxxx", "neitherthisonexxx")
  res <- synchronise(do(bad))
  expect_equal(res$ref, bad)
  expect_equal(res$type, c("cran", "standard"))
  expect_equal(res$direct, c(TRUE, TRUE))
  expect_equal(res$status, c("FAILED", "FAILED"))
})

test_that("dependency types", {
  conf <- current_config()
  conf$set("dependencies", TRUE)
  cache <- list(
    package = pkgcache::package_cache$new(),
    metadata = pkgcache::get_cranlike_metadata_cache())
  do <- function(refs) {
    res <- new_resolution(config = conf, cache = cache)
    res$push(.list = parse_pkg_refs(refs), direct = TRUE)
    res$when_complete()
  }

  res <- synchronise(do("processx"))
  dt <- res$dep_types
  expect_equal(
    dt[res$package == "processx"],
    replicate(
      sum(res$package == "processx"),
      c("Depends", "Imports", "LinkingTo", "Suggests"),
      simplify = FALSE
    )
  )
  expect_equal(
    dt[res$package != "processx"],
    replicate(
      sum(res$package != "processx"),
      c("Depends", "Imports", "LinkingTo"),
      simplify = FALSE
    )
  )
})
