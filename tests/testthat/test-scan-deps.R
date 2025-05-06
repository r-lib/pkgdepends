test_that("scan_deps", {
  local_reproducible_output(width = 500)
  withr::local_envvar(R_PKG_CACHE_DIR = tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)

  project <- test_path("fixtures/scan/project-1")
  expect_snapshot(variant = .Platform$OS.type, {
    scan_deps(project, root = project)[]
  })
  expect_snapshot(variant = .Platform$OS.type, {
    scan_deps(project, root = project)
  })
})

test_that("scan_deps errors", {
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_dir(tmp)
  writeLines(c("Package: foo", "Version: 1.0.0"), "DESCRIPTION")
  mkdirp(c("foo", "bar", "foobar"))
  expect_snapshot(error = TRUE, {
    # invalid types
    scan_deps(1:10)
    scan_deps(root = mtcars)
    # root does not exist
    scan_deps(root = "sdfssdfsdf")
    # path does not exist
    scan_deps("sdfssdfsdf")
    scan_deps(c("sdfssdfsdf-1", "sdfssdfsdf-2"))
    # path are not inside root
    scan_deps("foo", "bar")
    scan_deps(c("foo", "foobar"), "bar")
  })
})

test_that("get_deps_cache_path", {
  local_reproducible_output(width = 500)
  withr::local_envvar(R_PKG_CACHE_DIR = tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)

  expect_snapshot(transform = transform_tempdir, {
    writeLines(get_deps_cache_path())
    writeLines(get_deps_cache_path("badcafe"))
  })
})

test_that("clear_deps_cache", {
  local_reproducible_output(width = 500)
  withr::local_envvar(R_PKG_CACHE_DIR = tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)

  cval <- get_deps_cache_path("badcafe")
  mkdirp(dirname(cval))
  file.create(cval)

  expect_snapshot({
    dir(tmp, recursive = TRUE)
  })

  clear_deps_cache()
  expect_snapshot({
    dir(tmp, recursive = TRUE)
  })
})

test_that("re_r_dep", {
  expect_snapshot({
    re_r_dep()
  })
})

test_that("scan_path_deps", {
  local_reproducible_output(width = 500)
  withr::local_envvar(R_PKG_CACHE_DIR = tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)

  rfile <- test_path("fixtures/scan/project-1/R/code.R")
  expect_snapshot(variant = .Platform$OS.type, {
    scan_path_deps(rfile)
  })

  # now from the cache
  fake(scan_path_deps, "re_r_dep", function(...) stop("no"))
  expect_snapshot(variant = .Platform$OS.type, {
    scan_path_deps(rfile)
  })
})

test_that("scan_deps_df", {
  expect_snapshot({
    scan_deps_df()
  })
})

test_that("scan_path_deps_do", {
  rfile <- test_path("fixtures/scan/project-1/R/code.R")
  nsfile <- test_path("fixtures/scan/NAMESPACE")
  expect_snapshot({
    scan_path_deps_do(readLines(rfile), basename(rfile))
    scan_path_deps_do(readLines(nsfile), basename(nsfile))
  })

  expect_snapshot(error = TRUE, {
    scan_path_deps_do("code", "foo.unknown")
  })
})

test_that("scan_path_deps_do_r", {
  local_reproducible_output(width = 500)
  rfile <- test_path("fixtures/scan/project-1/R/code.R")
  expect_snapshot({
    scan_path_deps_do_r(readLines(rfile), rfile)
  })
})

# test_that("scan_path_deps_do_pkg_hits", { })

test_that("scan_path_deps_do_fn_hits", {
  local_reproducible_output(width = 500)
  rfile <- test_path("fixtures/scan/methods.R")
  expect_snapshot({
    scan_path_deps_do_r(readLines(rfile), rfile)
  })
})

# test_that("scan_path_deps_do_gen_hits", { })

test_that("scan_path_deps_do_jr_hits", {
  local_reproducible_output(width = 500)
  rfile <- test_path("fixtures/scan/junit.R")
  expect_snapshot({
    scan_path_deps_do_r(readLines(rfile), rfile)
  })
})

test_that("scan_pat_deps_do_ragg_hits", {
  local_reproducible_output(width = 500)
  rfile <- test_path("fixtures/scan/knitr.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(rfile), rfile)
  })
  rfile <- test_path("fixtures/scan/noragg.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(rfile), rfile)
  })
})

# test_that("scan_pat_deps_do_db_hits", { })

test_that("safe_parse_pkg_from_call", {
  # error
  expect_null(
    safe_parse_pkg_from_call(NA_character_, "library", "library(error")
  )
  expect_snapshot(
    safe_parse_pkg_from_call(NA_character_, "library", "library(qwe)")
  )
})

# test_that("parse_pkg_from_call_match", { })

test_that("parse_pkg_from_call", {
  expect_snapshot({
    parse_pkg_from_call(NA_character_, "library", "library(qwe)")
    parse_pkg_from_call("base", "loadNamespace", "loadNamespace('q1')")
    parse_pkg_from_call(
      "base",
      "requireNamespace",
      "requireNamespace('q1')"
    )
    parse_pkg_from_call(
      NA_character_,
      "pkg_attach",
      "pkg_attach('foobar')"
    )
    parse_pkg_from_call(
      NA_character_,
      "pkg_attach2",
      "pkg_attach2('foobar')"
    )
    parse_pkg_from_call("pacman", "p_load", "p_load('p1')")
    parse_pkg_from_call(NA_character_, "import", "import(x1)")
    parse_pkg_from_call(NA_character_, "module", "module({import('x2')})")
    parse_pkg_from_call("import", "from", "import::from(dplyr)")
    parse_pkg_from_call(
      "import",
      "into",
      "import::into('operators', .from = 'dplyr')"
    )
    parse_pkg_from_call("import", "here", "import::here('dplyr')")
    parse_pkg_from_call("box", "use", "box::use(dplyr[filter, select])")
    parse_pkg_from_call(
      "targets",
      "tar_option_set",
      "tar_option_set(packages = c('p1', 'p2'))"
    )
    parse_pkg_from_call(
      "glue",
      "glue",
      "glue::glue('blah {library(x5)} blah')"
    )
    parse_pkg_from_call(
      NA_character_,
      "ggsave",
      "ggsave(filename = 'foo.svg')"
    )
    parse_pkg_from_call(
      NA_character_,
      "set_engine",
      "set_engine(engine = 'spark')"
    )
    parse_pkg_from_call(
      "R6",
      "R6Class",
      "R6::R6Class('foobar', inherit = JunitReporter)"
    )
    parse_pkg_from_call(
      "testthat",
      "test_dir",
      "testthat::test_dir(reporter = 'junit')"
    )
  })
})

test_that("parse_pkg_from_call_library", {
  ppcl <- function(fn, code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match(fn, code)
    parse_pkg_from_call_library(ns, fn, matched)
  }
  expect_null(
    ppcl("library", "library(qqq)", ns = "other")
  )
  expect_null(
    ppcl("library", "library(qqq, character.only = TRUE)")
  )
  expect_null(
    ppcl("require", "require(qqq)", ns = "other")
  )
  expect_null(
    ppcl("require", "require(qqq, character.only = TRUE)")
  )
  expect_snapshot({
    ppcl("library", "library(qqq)")
    ppcl("library", "library('qqq')")
    ppcl("library", "library(qqq)", ns = "base")
    ppcl("require", "require(qqq)")
    ppcl("require", "require('qqq')")
    ppcl("require", "require('qqq')", ns = "base")
  })
})

test_that("parse_pkg_from_call_loadnamespace", {
  ppcln <- function(fn, code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match(fn, code)
    parse_pkg_from_call_loadnamespace(ns, fn, matched)
  }
  expect_null(
    ppcln("loadNamespace", "loadNamespace('www')", ns = "other")
  )
  expect_null(
    ppcln("loadNamespace", "loadNamespace(www)")
  )
  expect_null(
    ppcln("loadNamespace", "loadNamespace(c('one', 'two'))")
  )
  expect_null(
    ppcln("loadNamespace", "loadNamespace(123)")
  )
  expect_equal(
    ppcln("loadNamespace", "loadNamespace('eee')"),
    "eee"
  )
  expect_equal(
    ppcln("loadNamespace", "loadNamespace('eee')", ns = "base"),
    "eee"
  )

  expect_null(
    ppcln("requireNamespace", "requireNamespace('www')", ns = "other")
  )
  expect_null(
    ppcln("requireNamespace", "requireNamespace(www)")
  )
  expect_null(
    ppcln("requireNamespace", "requireNamespace(c('one', 'two'))")
  )
  expect_null(
    ppcln("requireNamespace", "requireNamespace(123)")
  )
  expect_equal(
    ppcln("requireNamespace", "requireNamespace('eee')"),
    "eee"
  )
  expect_equal(
    ppcln("requireNamespace", "requireNamespace('eee')", ns = "base"),
    "eee"
  )
})

test_that("parse_pkg_from_call_xfun", {
  ppcx <- function(fn, code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match(fn, code)
    parse_pkg_from_call_xfun(ns, fn, matched)
  }
  expect_null(
    ppcx("pkg_attach", "pkg_attach('qwe')", ns = "nope")
  )
  expect_null(
    ppcx("pkg_attach", "pkg_attach()")
  )
  expect_equal(
    ppcx("pkg_attach", "pkg_attach('p1', 'p2', 'p3')"),
    c("p1", "p2", "p3")
  )
  expect_equal(
    ppcx("pkg_attach2", "pkg_attach2('p1', 'p2', 'p3')"),
    c("p1", "p2", "p3")
  )
})

test_that("parse_pkg_from_call_pacman", {
  ppcp <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("p_load", code)
    parse_pkg_from_call_pacman(ns, fn, matched)
  }
  expect_null(ppcp("p_load()", ns = "foo"))
  expect_null(ppcp("p_load(xx, character.only = TRUE)"))
  expect_equal(ppcp("p_load(p1, 'p2', p3)"), c("p1", "p2", "p3"))
  expect_equal(ppcp("p_load(char = 'pp')"), 'pp')
  expect_equal(ppcp("p_load(char = c('p1', 'p2'))"), c("p1", "p2"))
})

test_that("parse_pkg_from_call_modules_import", {
  ppcmi <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("import", code)
    parse_pkg_from_call_modules_import(ns, fn, matched)
  }
  expect_null(ppcmi("import('pp')", ns = "foo"))
  expect_null(ppcmi("import(NULL)"))
  expect_equal(ppcmi("import('pp')"), 'pp')
  expect_equal(ppcmi("import(pp)"), 'pp')
})

test_that("parse_pkg_from_call_modules_module", {
  ppcmm <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("module", code)
    parse_pkg_from_call_modules_module(ns, fn, matched)
  }
  expect_null(ppcmm("module(x)", ns = "foo"))
  expect_null(ppcmm('module({})'))
  expect_equal(
    ppcmm(
      "module({
      # other expressions, mixed with import()
      pkg::fun()
      blah + blah
      import(p1)
      baaaaah
      import('p2')
    })"
    ),
    c('p1', 'p2')
  )
})

test_that("parse_pkg_from_call_import", {
  ppci <- function(fn, code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match(fn, code)
    parse_pkg_from_call_import(ns, fn, matched)
  }
  expect_null(ppci("from", "import::from(foo)", ns = "xx"))
  expect_equal(ppci("from", "import::from(foo)"), "foo")
  expect_equal(ppci("from", "import::from('foo')"), "foo")
  expect_equal(ppci("here", "import::here(foo)"), "foo")
  expect_equal(ppci("here", "import::here('foo')"), "foo")
  expect_equal(ppci("into", "import::into(.from = foo)"), "foo")
  expect_equal(ppci("into", "import::into(.from = 'foo')"), "foo")
  expect_null(ppci("from", "import::from(xx, .character_only = TRUE)"))
  expect_null(ppci("from", "import::from('./path.R')"))
})

test_that("parse_pkg_from_call_box", {
  ppcb <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("use", code)
    parse_pkg_from_call_box(ns, fn, matched)
  }
  expect_null(ppcb("box::use(pkg)", ns = 'not'))
  expect_null(ppcb("box::use(foo/bar)"))
  expect_null(ppcb("box::use(.[ff])"))
  expect_null(ppcb("box::use(..[ff])"))
  expect_equal(ppcb("box::use(pkg)"), "pkg")
  expect_equal(ppcb("box::use(pkg[f1, f2])"), c("pkg"))
  expect_equal(ppcb("box::use(pkg0, pkg[f1, f2])"), c("pkg0", "pkg"))
})

test_that("parse_pkg_from_call_targets", {
  ppct <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("tar_option_set", code)
    parse_pkg_from_call_targets(ns, fn, matched)
  }
  expect_null(ppct("tar_option_set(packages = 'pp')", ns = 'not'))
  expect_null(ppct("tar_option_set()"))
  expect_equal(
    ppct("tar_option_set(packages = { 1:10; c('p1', 'p2') })"),
    c("p1", "p2")
  )
})

test_that("dependencies_eval", {
  expect_snapshot({
    dependencies_eval(quote({
      1:10
      c(10:1)[1:3]
    }))
  })
})

test_that("parse_pkg_from_call_glue", {
  ppcg <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("glue", code)
    parse_pkg_from_call_glue(ns, fn, matched)
  }
  expect_null(ppcg("glue('{library(xx)}')", ns = "nope"))
  expect_null(ppcg("glue('no code at all')"))
  expect_equal(
    ppcg("glue('some {library(p1)} code {p2::f()}')"),
    c("p1", "p2")
  )
})

test_that("parse_pkg_from_call_ggplot2", {
  ppcgg <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("ggsave", code)
    parse_pkg_from_call_ggplot2(ns, fn, matched)
  }
  expect_null(ppcgg("ggsave(filename = 'foo.svg')", ns = 'not'))
  expect_null(ppcgg("ggsave(filename = 'foo.png')"))
  expect_null(ppcgg("ggsave(filename = var)"))
  expect_equal(ppcgg("ggsave(filename = 'foo.svg')"), "svglite")
})

test_that("parse_pkg_from_call_parsnip", {
  ppcp <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("set_engine", code)
    parse_pkg_from_call_parsnip(ns, fn, matched)
  }
  expect_null(ppcp("set_engine(engine = 'keras')", ns = "nope"))
  expect_null(ppcp("set_engine()"))
  withr::local_options(renv.parsnip.engines = NULL)
  expect_equal(ppcp("set_engine(engine = 'glm')"), "stats")
  withr::local_options(renv.parsnip.engines = list(foo = "bar"))
  expect_equal(ppcp("set_engine(engine = 'foo')"), "bar")
  withr::local_options(renv.parsnip.engines = function(x) "eng")
  expect_equal(ppcp("set_engine(engine = 'foo')"), "eng")
  withr::local_options(renv.parsnip.engines = function(x) NULL)
  expect_null(ppcp("set_engine(engine = 'foo')"))
})

test_that("parse_pkg_from_call_testthat_r6class", {
  ppcttr6 <- function(code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match("R6Class", code)
    parse_pkg_from_call_testthat_r6class(ns, fn, matched)
  }
  expect_null(ppcttr6("R6Class(inherit = JunitReporter)", ns = 'not'))
  expect_null(ppcttr6("R6Class(inherit = someother)"))
  expect_equal(ppcttr6("R6Class(inherit = JunitReporter)"), "xml2")
  expect_equal(
    ppcttr6("R6Class(inherit = testthat::JunitReporter)"),
    "xml2"
  )
})

test_that("parse_pkg_from_call_testthat_test", {
  ppcttt <- function(fn, code, ns = NA_character_) {
    matched <- parse_pkg_from_call_match(fn, code)
    parse_pkg_from_call_testthat_test(ns, fn, matched)
  }
  expect_null(
    ppcttt("test_dir", "test_dir(reporter = 'junit')", ns = "other")
  )
  expect_null(
    ppcttt("test_dir", "test_dir(reporter = 'other')")
  )
  expect_equal(
    ppcttt("test_dir", "test_dir(reporter = 'junit')"),
    "xml2"
  )
})

test_that("scan_path_deps_do_rmd", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/chunk-errors.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), "chunk-errors.Rmd")
  })
})

test_that("scan_path_deps_do_rmd #2", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/inline-chunks.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), "inline-chunks.Rmd")
  })
})

test_that("scan_path_deps_do_rmd #3", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/nothing.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), "nothing.Rmd")
  })
})

# test_that("scan_path_deps_do_inline_hits", { })
# test_that("scan_path_deps_do_block_hits", { })

test_that("scan_path_deps_do_header_hits", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/header.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), basename(path))
  })
})

test_that("scan_path_deps_do_header_shiny_hits", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/header-shiny.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), basename(path))
  })
  path <- test_path("fixtures/scan/header-shiny2.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), basename(path))
  })
})

test_that("scan_path_deps_do_header_bslib_hits", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/header-bslib.Rmd")
  expect_snapshot({
    scan_path_deps_do_rmd(readLines(path), basename(path))
  })
})

test_that("scan_path_deps_do_dsc", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/DESCRIPTION")
  expect_snapshot({
    print(scan_path_deps_do_dsc(readLines(path), basename(path)), n = Inf)
  })
})

test_that("scan_path_deps_do_namespace", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/NAMESPACE")
  expect_snapshot({
    print(
      scan_path_deps_do_namespace(
        readBin(path, "raw", 10000),
        path
      ),
      n = Inf
    )
  })
})

test_that("scan_path_deps_do_{bookdown,pkgdown,quarto,renv_lock,rsconnect}", {
  local_reproducible_output(width = 500)
  withr::local_dir(test_path("fixtures/scan/project-2"))
  expect_snapshot({
    scan_deps()[]
  })
})

test_that(".Rproj file", {
  local_reproducible_output(width = 500)
  project <- test_path("fixtures/scan/project-3")
  expect_snapshot({
    scan_deps(project, root = project)[]
  })
})

test_that("scan_path_deps_do_rnw_ranges", {
  path <- test_path("fixtures/scan/test.Rnw")
  code <- readLines(path)
  expect_snapshot({
    scan_path_deps_do_rnw_ranges(code)
  })
})

test_that("scan_path_deps_do_rnw_parse_chunk_header", {
  expect_snapshot({
    scan_path_deps_do_rnw_parse_chunk_header("")
    scan_path_deps_do_rnw_parse_chunk_header("name")
    scan_path_deps_do_rnw_parse_chunk_header(
      "name, foo = 1, bar = TRUE, this = that"
    )
  })
})

test_that(".Rnw file", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/test.Rnw")
  expect_snapshot({
    scan_path_deps_do_rnw(
      readLines(path),
      basename(path)
    )
    scan_path_deps_do(
      readLines(path),
      basename(path)
    )
  })
})

test_that("Ignored chunks in .Rnw file", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/ignore-test.Rnw")
  expect_snapshot({
    scan_path_deps_do(
      readBin(path, "raw", file.size(path)),
      basename(path)
    )
  })
})

test_that("IPython notebook", {
  local_reproducible_output(width = 500)
  path <- test_path("fixtures/scan/notebook.ipynb")
  expect_snapshot({
    scan_path_deps_do(readLines(path), basename(path))
  })
})
