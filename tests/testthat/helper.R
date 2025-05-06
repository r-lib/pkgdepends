expect_equal_named_lists <- function(object, expected, ...) {
  expect_true(!is.null(names(object)) && !is.null(names(expected)))
  expect_true(is.list(object) && is.list(expected))
  object <- object[order(names(object))]
  expected <- expected[order(names(expected))]
  expect_equal(object, expected)
}

test_temp_file <- function(
  fileext = "",
  pattern = "test-file-",
  envir = parent.frame(),
  create = TRUE
) {
  tmp <- tempfile(pattern = pattern, fileext = fileext)
  if (identical(envir, .GlobalEnv)) {
    message("Temporary files will _not_ be cleaned up")
  } else {
    withr::defer(
      try(unlink(tmp, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir
    )
  }
  if (create) {
    cat("", file = tmp)
    normalizePath(tmp)
  } else {
    tmp
  }
}

test_temp_dir <- function(pattern = "test-dir-", envir = parent.frame()) {
  tmp <- test_temp_file(pattern = pattern, envir = envir, create = FALSE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  normalizePath(tmp)
}

# Create a temporary directory and switch the working directory to it
local_temp_dir <- function(..., .local_envir = parent.frame()) {
  tmp <- withr::local_tempdir(..., .local_envir = .local_envir)
  withr::local_dir(tmp, .local_envir = .local_envir)
  invisible(tmp)
}

test_package_root <- function() {
  x <- tryCatch(
    find_package_root(),
    error = function(e) NULL
  )

  if (!is.null(x)) return(x)

  pkg <- testthat::testing_package()
  x <- tryCatch(
    find_package_root(
      path = file.path("..", "..", "00_pkg_src", pkg)
    ),
    error = function(e) NULL
  )

  if (!is.null(x)) return(x)

  stop("Cannot find package root")
}

skip_in_covr <- function() {
  if (Sys.getenv("R_COVR") == "true") skip("In covr")
}

# TODO: update this to cli.num_colors

local_cli_config <- function(
  unicode = FALSE,
  dynamic = FALSE,
  ansi = FALSE,
  num_colors = 1,
  .local_envir = parent.frame()
) {
  withr::local_options(
    cli.dynamic = dynamic,
    cli.ansi = ansi,
    cli.unicode = unicode,
    crayon.enabled = num_colors > 1,
    crayon.colors = num_colors,
    .local_envir = .local_envir
  )
  withr::local_envvar(
    PKG_OMIT_TIMES = "true",
    PKG_OMIT_SIZES = "true",
    .local_envir = .local_envir
  )
}

pst <- function(...) suppressMessages(...)

long_basename <- function(x) {
  # remove potential trailing slash
  l <- nchar(x)
  x <- ifelse(substr(x, l, l) %in% c("/", "\\"), substr(x, 1, l - 1), x)
  sub("^.*[/\\]", "", x)
}

read_all <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  chr <- rawToChar(bytes)
  Encoding(chr) <- "UTF-8"
  chr
}
