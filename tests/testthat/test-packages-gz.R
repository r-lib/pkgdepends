
context("packages-gz")

test_that("packages_make_target", {

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), NULL, NULL),
    c("src/contrib/p1_1.0.tar.gz", "src/contrib/p2_2.0.tar.gz")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), c("foo", "bar"), NULL),
    c("src/contrib/foo", "src/contrib/bar")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), c("foo", "bar"), c("1", "2")),
    c("src/contrib/foo", "src/contrib/bar")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2"),
                         c("1.0", "2.0"), NULL, c("foo", "bar")),
    c("src/contrib/foo/p1_1.0.tar.gz", "src/contrib/bar/p2_2.0.tar.gz")
  )

  expect_equal(
    packages_make_target("source", "src/contrib", c("p1", "p2", "p3"),
                         c("1.0", "2.0", "3.0"), c("foo", NA, NA),
                         c("foox", "bar", NA)),
    c("src/contrib/foo", "src/contrib/bar/p2_2.0.tar.gz",
      "src/contrib/p3_3.0.tar.gz")
  )
})

test_that("packages_make_sources", {

  expect_equal(
    packages_make_sources(
      "URL", "macos", c("s/c/p1_1.0.tgz", "s/c/p2_2.0.tgz"), "s/c",
      c("p1", "p2"), c("1.0", "2.0"), type = "cran"),
    list("URL/s/c/p1_1.0.tgz", "URL/s/c/p2_2.0.tgz")
  )

  expect_equal(
    packages_make_sources(
      "URL", "source", c("s/c/xx.tar.gz", "s/c/yy.tar.gz"), "s/c",
      c("p1", "p2"), c("1.0", "2.0"), type = "cran"),
    list(c("URL/s/c/xx.tar.gz", "URL/s/c/Archive/p1_1.0.tar.gz"),
         c("URL/s/c/yy.tar.gz", "URL/s/c/Archive/p2_2.0.tar.gz"))
  )
})

test_that("read_packages_file", {

  pkg_files <- vcapply(
    paste0("PACKAGES-", c("src", "win", "mac"), ".gz"),
    get_fixture)

  for (pf in pkg_files) {
    pkgs <- read_packages_file(
      pf, mirror = "mirror", repodir = "src/contrib", platform = "source",
      rversion = "rversion")
    check_packages_data(pkgs)
  }
})

test_that("packages_parse_deps", {
  pkgs <- read_packages_file(
    get_fixture("PACKAGES-src.gz"), mirror = "mirror",
    repodir = "src/contrib", platform = "source", rversion = "*")

  pkgs1 <- pkgs$pkgs[1,]
  deps <- packages_parse_deps(pkgs1)
  expect_true(inherits(deps, "tbl_df"))
  expect_equal(
    colnames(deps),
    c("upstream", "idx", "ref", "type", "package", "op",  "version"))
})

test_that("merge_packages_data", {
  pf <- vcapply(
    paste0("PACKAGES-", c("src", "win", "mac"), ".gz"),
    get_fixture)

  pkgsx <- list(
    read_packages_file(pf[1], mirror = "m1", repodir = "r1",
                       platform = "source", rversion = "*"),
    read_packages_file(pf[2], mirror = "m2", repodir = "r2",
                       platform = "windows", rversion = "3.4"),
    read_packages_file(pf[3], mirror = "m3", repodir = "r3",
                       platform = "macos", rversion = "3.5")
  )

  pkgs <- merge_packages_data(.list = pkgsx)

  check_packages_data(pkgs)

  expect_equal(
    nrow(pkgs$pkgs),
    nrow(pkgsx[[1]]$pkgs) + nrow(pkgsx[[2]]$pkgs) + nrow(pkgsx[[3]]$pkgs))
  expect_equal(
    nrow(pkgs$deps),
    nrow(pkgsx[[1]]$deps) + nrow(pkgsx[[2]]$deps) + nrow(pkgsx[[3]]$deps))
  expect_true(
    all(pkgs$pkgs$package[pkgs$deps$idx] == pkgs$deps$upstream))
})

test_that("rbind_expand", {
  d1 <- tibble::tibble(a = 1:2, b = c("a", "b"), c = NA_character_)
  d2 <- tibble::tibble(a = 3:4, c = c("c", "d"), d = c(1L, 2L))
  m <- rbind_expand(d1, d2)
  expect_identical(names(m), c("a", "b", "c", "d"))
  expect_identical(m$a, 1:4)
  expect_identical(m$b, c("a", "b", NA, NA))
  expect_identical(m$c, c(NA, NA, "c", "d"))
  expect_identical(m$d, c(NA, NA, 1L, 2L))
})
