
context("resolution-df")

test_that("res_make_empty_df", {
  df <- res_make_empty_df()
  expect_true(tibble::is_tibble(df))
  expect_equal(nrow(df), 0L)
})

test_that("res_df_defaults", {
  empty <- res_make_empty_df()
  def <- res_df_defaults()
  expect_true(all_named(def))
  expect_true(all(names(def) %in% names(empty)))

  def_types <- vcapply(def, class)
  empty_types <- res_df_entry_types()[names(def)]
  expect_true(all(def_types == empty_types | def_types == "call"))

  lengths <- viapply(def, length)
  expect_true(all(lengths == 1L | vcapply(def, class) == "call"))
})

test_that("res_df_entry_types", {
  types <- res_df_entry_types()
  expect_true(all_named(types))
  expect_true(is.character(types))
  expect_false(any(types == ""))
})

test_that("res_df_must_have", {
  types <- res_df_must_have()
  def <- res_df_defaults()
  expect_true(length(intersect(types, def)) == 0)
})

test_that("res_check_entry", {
  good <- list(
    ref = "package",
    type = "standard",
    package = "package",
    version = "1.0.0",
    sources = list("url1", "url2"),
    remote = quote(parse_remotes(ref))
  )

  expect_error(ent <- res_check_entry(good), NA)

  bad <- c(good, list("noname"))
  expect_error(res_check_entry(bad), "must be a list of named entries")

  bad <- unlist(good[1:3])
  expect_error(res_check_entry(bad), "not a list")

  bad <- modifyList(good, list(type = 1L))
  expect_error(res_check_entry(bad), "Wrong entry types")
})

test_that("res_add_df_entries", {
  good <- list(
    ref = "package",
    type = "standard",
    package = "package",
    version = "1.0.0",
    sources = list(c("url1", "url2"))
  )

  empty <- res_make_empty_df()

  df <- res_add_df_entries(empty, list(good, good))
  expect_identical(names(df), names(empty))
  expect_equal(nrow(df), 2)
})
