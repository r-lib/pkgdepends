
test_that("make_bar", {
  chars <- withr::with_options(list(cli.unicode = FALSE), progress_chars())
  lapply(
    seq(0, 1, by = 0.2),
    function(p) {
      expect_equal(nchar(crayon::strip_style(make_bar(chars, p, 10))), 10)
    }
  )
})

test_that("initial state", {
  local_cli_config()

  # num = 0, nch = 0, cbt = 0
  # no downloads are needed, all installed
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "installed",      10000, "foo",    NA_character_,
    "installed",      10000, "foo2",   "miss",
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num = 0, nch = 1, cbt > 0
  # no downloads needed, all cached or installed
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "hit",
    "installed",      10000, "foo2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num = 0, nch = 1, cbt > 0
  # no downloads needed, but cached size known
  what <- tibble::tribble(
    ~type,        ~filesize,       ~package, ~cache_status,
    "cran",           10000,       "foo",    "hit",
    "cran",           10000,       "foo2",   "hit",
    "installed",      10000,       "foo3",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num = 0, nch = 1, cbt = 0
  # no downloads needed, cached size unknown
  what <- tibble::tribble(
    ~type,        ~filesize,       ~package, ~cache_status,
    "cran",           NA_integer_, "foo",    "hit",
    "installed",      10000,       "foo2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts > 0, unk = 0, nch = 0, cbt = 0
  # nothing is cached, sizes known
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "cran",           20000, "bar",    "miss"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts > 0, unk = 0, nch = 1, cbt = 0
  # 1 package cached, 2 downloaded, cached size unknown
  what <- tibble::tribble(
    ~type,         ~filesize, ~package, ~cache_status,
    "cran",            10000, "foo",    "miss",
    "cran",            20000, "bar",    "miss",
    "installed",       10000, "foo2",   NA_character_,
    "cran",      NA_integer_, "bar2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts > 0, unk = 0, nch = 1, cbt > 0
  # 1 package cached, 2 downloaded, cached size known
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "cran",           20000, "bar",    "miss",
    "installed",      10000, "foo2",   NA_character_,
    "cran",           20000, "bar2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts > 0, unk > 0, nch = 0, cbt = 0
  # downloads with unknown sizes
  what <- tibble::tribble(
    ~type,         ~filesize, ~package, ~cache_status,
    "cran",            20000, "foo",    "miss",
    "cran",      NA_integer_, "bar",    "miss",
    "installed",       10000, "foo2",   NA_character_
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts > 0, unk > 0, nch = 1, cbt > 0
  # downloads with known & unknown sizes, 1 cached with known size
  what <- tibble::tribble(
    ~type,         ~filesize,       ~package, ~cache_status,
    "cran",            10000,       "foo",    "miss",
    "cran",      NA_integer_,       "bar",    "miss",
    "installed",       10000,       "foo2",   NA_character_,
    "cran",            30000,       "bar2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts > 0, unk > 0, nch = 1, cbt = 0
  # downloads with known sizes, 1 cached with unknown size
  what <- tibble::tribble(
    ~type,         ~filesize, ~package, ~cache_status,
    "cran",            10000, "foo",    "miss",
    "cran",      NA_integer_, "bar",    "miss",
    "installed",       10000, "foo2",   NA_character_,
    "cran",      NA_integer_, "bar2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts = 0, unk > 0, nch = 0, cbt = 0
  # downloads with only unknown sizes, nothing cached
  what <- tibble::tribble(
    ~type,         ~filesize, ~package, ~cache_status,
    "cran",      NA_integer_, "foo",    "miss",
    "cran",      NA_integer_, "bar",    "miss",
    "installed",      10000,  "foo2",   NA_character_
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts = 0, unk > 0, nch = 1, cbt = 0
  # downloads with unknown sizes, 1 cached with unknown size
  what <- tibble::tribble(
    ~type,         ~filesize, ~package, ~cache_status,
    "cran",      NA_integer_, "foo",    "miss",
    "cran",      NA_integer_, "bar",    "miss",
    "installed",       10000, "foo2",   NA_character_,
    "cran",      NA_integer_, "bar2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))

  # num > 0, bts = 0, unk > 0, nch = 1, cbt > 0
  # downloads with unknown sizes, 1 cached
  what <- tibble::tribble(
    ~type,         ~filesize, ~package, ~cache_status,
    "cran",      NA_integer_, "foo",    "miss",
    "cran",      NA_integer_, "bar",    "miss",
    "installed",       10000, "foo2",   NA_character_,
    "cran",            30000, "bar2",   "hit"
  )
  expect_snapshot(invisible(
    pkgplan__create_progress_bar(what)
  ))
})

test_that("data updates", {
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "installed",      10000, "foo2",   "miss",    # no download, installed
    "cran",           20000, "bar",    "hit",     # no download, cached
    "cran",           20000, "bar2",   "miss"
  )

  expect_snapshot({
    bar <- pkgplan__create_progress_bar(what = what)
    pkgplan__update_progress_bar(bar, 1L, "data", list(current = 5000, total = 10000))
    pkgplan__update_progress_bar(bar, 4L, "data", list(current = 20000, total = 20000))
    pkgplan__done_progress_bar(bar)
    bar$what
  })
})

test_that("all finish messages for updates", {
  do <- function(idx, event = "done", download_status = "Got",
                 make_tempfile = TRUE) {
    bar <- pkgplan__create_progress_bar(what = what)
    tmp <- tempfile()
    if (make_tempfile) {
      writeBin(raw(7), tmp)
      on.exit(unlink(tmp), add = TRUE)
    }
    pkgplan__update_progress_bar(
      bar, idx, event,
      list(
        download_status = download_status,
        package = "foo",
        platform = "source",
        version = "1.0.0",
        fulltarget = tmp,
        fulltarget_tree = tempfile()
      )
    )
    pkgplan__done_progress_bar(bar)
    bar$what
  }

  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "installed",      10000, "foo2",   "miss",    # no download, installed
    "cran",           20000, "bar",    "hit",     # no download, cached
    "cran",           20000, "bar2",   "miss"
  )

  expect_snapshot(do(1L))
  expect_snapshot(do(1L, make_tempfile = FALSE))
  expect_snapshot(do(1L, event = "error"))
})

test_that("rate", {
  now <- Sys.time()
  chunks <- as.environment(list(
    "0" = 1000,
    "1" = 2000,
    "2" = 0,
    "3" = 400,
    "4" = 100,
    "5" = 200
  ))

  r <- calculate_rate(now - 5.5, now, chunks)
  expect_equal(r, list(rate = 200, rstr = "0.2 kB/s"))
})

test_that("eta", {
  expect_equal(
    calculate_eta(100, 50, 0),
    list(etas = NA, estr = "??s ")
  )
  expect_equal(
    calculate_eta(100, 50, 10),
    list(etas = as.difftime(5, units = "secs"), estr = "~5s   ")
  )
})

test_that("parts are calculated properly", {
  local_cli_config()
  what <- tibble::tribble(
    ~type,        ~filesize, ~package, ~cache_status,
    "cran",           10000, "foo",    "miss",
    "cran",           20000, "bar",    "miss",
    "installed",      10000, "foo2",   NA_character_,
    "cran",           20000, "bar2",   "hit"
  )

  expect_snapshot(
    bar <- pkgplan__create_progress_bar(what)
  )
  parts <- calculate_progress_parts(bar)
  expect_equal(parts$pkg_done, "0")
  expect_equal(parts$pkg_total, "2")
  expect_equal(parts$percent, "  0%")
  expect_match(parts$rate, "^\\s+$")
  expect_equal(parts$msg, "Connecting...")
  expect_match(crayon::strip_style(parts$line), "^[(][\\s\u00a0]+[)]$", perl = TRUE)
  expect_equal(parts$eta, "??s ")
})

test_that("parts if file size is unknown", {
  # TODO
  expect_true(TRUE)
})

test_that("status bar is cleaned up on interrupt / error", {
  # TODO
  expect_true(TRUE)
})
