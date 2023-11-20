
test_that("%|z|%", {
  expect_true("" %|z|% TRUE)

  bad <- list(
    character(),
    c("", ""),
    "foo",
    structure("", class = "foo")
  )
  for (b in bad) expect_identical(b %|z|% FALSE, b)
})

test_that("%&z&%", {
  expect_equal(
    NULL %&z&% "bar",
    ""
  )
  expect_equal(
    character() %&z&% "bar",
    ""
  )
  expect_equal(
    "" %&z&% "bar",
    ""
  )
  expect_equal(
    "foo" %&z&% "bar",
    "bar"
  )
})

test_that("get_private", {
  cls <- R6::R6Class(
    "foo",
    public = list(foo = function() "foo"),
    private = list(bar = function() "bar")
  )
  obj <- cls$new()
  expect_equal(get_private(obj)$bar(), "bar")
})

test_that("default_cran_mirror", {
  m1 <- withr::with_options(
    list(repos = c(CRAN = "@CRAN@")),
    default_cran_mirror()
  )
  m2 <- withr::with_options(
    list(repos = NULL),
    default_cran_mirror()
  )
  m3 <- withr::with_options(
    list(repos = c("foo" = "bar")),
    default_cran_mirror()
  )

  expect_true(is.character(m1) && length(m1) == 1 && !is.na(m1))
  expect_identical(m1, m2)
  expect_identical(m1, m3)

  m4 <- withr::with_options(
    list(repos = c(CRAN = "mymirror")),
    default_cran_mirror()
  )
  expect_identical(m4, c(CRAN = "mymirror"))
})

test_that("current_r_version", {
  ver <- current_r_version()
  expect_true(is.character(ver))
  expect_true(length(ver) == 1)
})

test_that("get_minor_r_version", {
  expect_equal(get_minor_r_version("4.2.2"), "4.2")
  expect_equal(get_minor_r_version("4.2.2.0"), "4.2")
})

test_that("recommended_packages", {
  expect_snapshot(
    recommended_packages()
  )
})

test_that("lapply_with_names", {
  expect_equal(
    lapply_with_names(list(a = 1, b = 2), function(x) x * 2),
    list(a = 2, b = 4)
  )
  expect_equal(
    lapply_with_names(c("a","b"), function(x) paste0(x, x)),
    list(a = "aa", b = "bb")
  )
})

test_that("vlapply", {
  l <- list(NULL, "", character(), 1)
  expect_identical(
    vapply(l, is.character, logical(1)),
    vlapply(l, is.character)
  )
  expect_identical(
    vapply(list(), is.character, logical(1)),
    vlapply(list(), is.character)
  )
  expect_error(vlapply(l, identity), "values must be length 1")
  expect_error(vlapply(1:5, identity), "values must be type .*logical")
})

test_that("viapply", {
  expect_equal(
    viapply(c(1L, 2L, 3L), function(x) x),
    c(1L, 2L, 3L)
  )
  expect_equal(
    viapply(c(a = 1L, b = 2L, c = 3L), function(x) x),
    c(a = 1L, b = 2L, c = 3L)
  )
  expect_error(
    viapply(c(a = 1L, b = 2L), function(x) 1)
  )
})

test_that("vdapply", {
  l <- list(NULL, "", character(), 1)
  f <- function(x) as.double(length(x))
  expect_identical(
    vapply(l, f, double(1)),
    vdapply(l, f)
  )
  expect_identical(
    vapply(list(), f, double(1)),
    vdapply(list(), f)
  )
  expect_error(vdapply(l, identity), "values must be length 1")
  expect_error(vdapply(letters, identity), "values must be type .*double")
})

test_that("add_class", {
  expect_equal(
    add_class(structure(1, class = "foo"), "bar"),
    structure(1, class = c("bar", "foo"))
  )
})

test_that("cat0", {
  expect_snapshot({
    cat0("foo", "bar", "\n")
    cat0("foo", "bar", "\n",  sep = " ")
  })
})

test_that("lapply_rows", {
  expect_equal(
    lapply_rows(data_frame(), function(...) 1),
    list()
  )
  expect_equal(
    lapply_rows(
      data_frame(foo = character(), bar = integer()),
      function(...) 1
    ),
    list()
  )
  expect_snapshot(
    lapply_rows(mtcars[1:3,], function(row) row)
  )
})

test_that("detect_download_cache_dir", {
  expect_equal(
    detect_download_cache_dir(),
    detect_download_cache_dir()
  )
})

test_that("rbind_expand", {
  expect_snapshot({
    rbind_expand(data_frame(), data_frame())
    rbind_expand(data_frame(), data_frame(foo = 1:2))
    rbind_expand(data_frame(bar = c("a", "b")), data_frame())
    rbind_expand(data_frame(foo = 1:2), data_frame(foo = 3:4))
    rbind_expand(data_frame(foo = 1:2), data_frame(bar = 3:4))
    rbind_expand(
      data_frame(foo = list(1,2), bar = letters[1:2]),
      data_frame(foo = list(3,4), baz = list("x", "y"))
    )
  })
})

test_that("drop_nulls", {
  expect_equal(drop_nulls(list()), list())
  expect_equal(drop_nulls(list(1,2,3)), list(1,2,3))
  expect_equal(drop_nulls(list(NULL)), list())
  expect_equal(drop_nulls(list(NULL, NULL)), list())
  expect_equal(drop_nulls(list(NULL, 1, NULL)), list(1))
})

test_that("get_num_workers", {
  withr::local_options(Ncpus = NULL)

  # ps is not installed
  mockery::stub(get_num_workers, "ps::ps_cpu_count", function(...) stop("no"))
  expect_equal(get_num_workers(), 1L)

  # ps works
  mockery::stub(get_num_workers, "ps::ps_cpu_count", function(...) 13L)
  expect_equal(get_num_workers(), 13L)

  # option works
  withr::local_options(Ncpus = 11L)
  expect_equal(get_num_workers(), 11L)
})

test_that("is_rcmd_check", {
  withr::local_envvar(NOT_CRAN = "true")
  expect_false(is_rcmd_check())

  withr::local_envvar(
    NOT_CRAN = NA_character_,
    "_R_CHECK_PACKAGE_NAME_" = NA_character_
  )
  expect_false(is_rcmd_check())

  withr::local_envvar(
    NOT_CRAN = NA_character_,
    "_R_CHECK_PACKAGE_NAME_" = "foo"
  )
  expect_true(is_rcmd_check())
})

test_that("update_named_vector", {
  cases <- list(
    list(c(a=1, b=2), c(a=2, c=5), c(a=2, b=2, c=5)),
    list(double(), c(a=2), c(a=2)),
    list(character(), character(), character()),
    list(c(a=1), double(), c(a=1))
  )

  for (c in cases) {
    expect_identical(update_named_vector(c[[1]], c[[2]]), c[[3]])
  }

  expect_error(update_named_vector(1, c(a=1)), "must be named.")
  expect_error(update_named_vector(c(a=1), 1), "must be named.")
})

test_that("make_dl_status", {
  obj <- list(
    status = "status",
    url = "url",
    target = "target",
    bytes = NA_real_,
    error = NULL
  )

  expect_identical(
    make_dl_status("Got", obj$url, obj$target, 100L),
    update_named_vector(obj, list(status = "Got", bytes = 100))
  )

  expect_identical(
    make_dl_status("Failed", obj$url, obj$target, error = "foobar"),
    update_named_vector(obj, list(status = "Failed", error = "foobar"))
  )

  expect_identical(
    make_dl_status("Had", obj$url, obj$target, 100),
    update_named_vector(obj, list(status = "Had", bytes = 100))
  )

})

test_that("comma_wrap", {
  expect_equal(
    withr::with_options(
      list(width = 15),
      comma_wrap(c("foo", "x", "foo2"))
    ),
    "  foo, x,\n  foo2"
  )

  expect_equal(
    withr::with_options(
      list(width = 10),
      comma_wrap(c("foo", "x", "foo2"), indent = 0)
    ),
    "foo, x,\nfoo2"
  )

  expect_equal(
    withr::with_options(
      list(width = 10),
      comma_wrap(c("foo", "x", "foo2"), indent = 0, exdent = 2)
    ),
    "foo, x,\n  foo2"
  )

  expect_equal(
    withr::with_options(
      list(width = 15),
      comma_wrap(c("foo", "x", "foo2"), sep = "xx ")
    ),
    "  fooxx xxx\n  foo2"
  )
})

test_that("is_na_scalar", {
  pos <- list(NA, NA_character_, NA_real_, NA_integer_, NA_complex_)
  neg <- list(logical(), integer(), 1, 1L, NULL, "foobar", c(NA, 1))

  for (p in pos) expect_true(is_na_scalar(p))
  for (n in neg) expect_false(is_na_scalar(n))
})

test_that("omit_cols", {
  df <- data.frame(a = 1:5, b = 5:1, c = letters[1:5])
  expect_identical(omit_cols(df, character()), df)
  expect_identical(omit_cols(df, "x"), df)
  expect_identical(omit_cols(df, "a"), df[, 2:3])
  expect_identical(omit_cols(df, c("a", "b")), df[, 3, drop = FALSE])
  expect_identical(omit_cols(df, c("a", "b", "c")), df[, c(), drop = FALSE])
})

test_that("same_sha", {
  expect_true(same_sha("badcafe", "b"))
  expect_true(same_sha("b", "badcafe"))
  expect_false(same_sha("badcafe1", "badcafebadcafe"))
  expect_false(same_sha("badcafe", NA_character_))
  expect_false(same_sha(NA_character_, "badcafe"))
})

test_that("format_iso_8601", {
  d <- structure(1266510204, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(format_iso_8601(d), "2010-02-18T16:23:24+00:00")
})

test_that("is_online", {
  environment(is_online)$expires <- Sys.time() - 1
  on.exit(environment(is_online)$expires <- Sys.time() - 1, add = TRUE)
  mockery::stub(is_online, "is_rcmd_check", TRUE)
  expect_false(is_online())
  mockery::stub(is_online, "is_rcmd_check", FALSE)
  mockery::stub(is_online, "pingr::is_online", TRUE)
  expect_true(is_online())
  mockery::stub(is_online, "pingr::is_online", FALSE)
  # cached for 10 minutes
  expect_true(is_online())
  environment(is_online)$expires <- Sys.time() - 1
  expect_false(is_online())
})

test_that("once_per_session", {
  once_per_session(reset = TRUE)
  on.exit(once_per_session(reset = TRUE), add = TRUE)
  expect_snapshot({
    once_per_session(message("hello"))
    once_per_session(message("hello"))
    once_per_session(reset = TRUE)
    once_per_session(message("hello"))
    once_per_session(message("hello"))
  })
})

test_that("format_error_with_stdout", {
  err <- new_error("message")
  expect_snapshot(format_error_with_stdout(err))

  err$stdout <- c("this is", "the", "standard output")
  expect_snapshot(format_error_with_stdout(err))
})

test_that("last_stdout_lines", {
  expect_snapshot({
    last_stdout_lines(letters[1:3], "stdout + stderr")
  })

  # truncated in interactive sessions
  withr:::local_options(rlib_interactive = TRUE)
  expect_snapshot({
    last_stdout_lines(letters[1:11], "stdout + stderr")
  })

  # full in non-interactive sessions
  withr:::local_options(rlib_interactive = FALSE)
  expect_snapshot({
    last_stdout_lines(letters[1:11], "stdout + stderr")
  })
})

test_that("is_windows", {
  expect_true(is_flag(is_windows()))
})

test_that("is_older_rstudio", {
  expect_true(is_flag(is_older_rstudio()))
  mockery::stub(is_older_rstudio, "rstudio$detect", list(type = "foo"))
  expect_false(is_older_rstudio())

  mockery::stub(
    is_older_rstudio,
    "rstudio$detect",
    list(type = "rstudio_console", version = package_version("1.4.801"))
  )
  expect_false(is_older_rstudio())

  mockery::stub(
    is_older_rstudio,
    "rstudio$detect",
    list(type = "rstudio_console", version = package_version("1.4.700"))
  )
  expect_true(is_older_rstudio())
})

cli::test_that_cli(configs = c("plain", "ansi"), "ansi_align_width", {
  expect_equal(ansi_align_width(character()), character())
  expect_snapshot(
    ansi_align_width(c("foobar", cli::col_red("bar")))
  )
})

test_that("get_id", {
  expect_true(is_count(get_id()))
  expect_true(get_id() != get_id())
})

test_that("safe_md5sum", {
  mkdirp(tmp <- withr::local_tempdir())
  x <- file.path(tmp, "cs\u0151\u00fa\u0171")
  file.create(x)
  expect_equal(
    safe_md5sum(x)[[1]],
    "d41d8cd98f00b204e9800998ecf8427e"
  )

  mockery::stub(
    safe_md5sum,
    "tools::md5sum",
    function(files) {
      if (files == x) stop("no") else tools::md5sum(files)
    }
  )
  expect_equal(
    safe_md5sum(x)[[1]],
    "d41d8cd98f00b204e9800998ecf8427e"
  )
})

test_that("get_euid", {
  skip_on_os("windows")
  expect_true(is_count(get_euid()))

  mockery::stub(get_euid, "processx::run", function(...) stop("nope"))
  expect_equal(get_euid(), NA_integer_)
})
