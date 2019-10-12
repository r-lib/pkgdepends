
fixtures <- list(

  "resolution-simple.rds" = function() {
    r <- pkg_plan$new("pkgconfig", lib = tempfile())
    r$resolve()
  },

  "resolution-progress.rds" = function() {
    r <- pkg_plan$new("progress", lib = tempfile())
    r$resolve()
  },

  "resolution-installed.rds" = function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    install.packages("pkgconfig", lib = tmp)
    r <- pkg_plan$new("pkgconfig", library = tmp)
    r$resolve()
  },

  "resolution-gh-vs-cran.rds" = function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    r <- pkg_plan$new(
      c("cran::pkgconfig", "github::r-lib/pkgconfig"),
      lib = tmp)
    r$resolve()
  },

  "solution-crayon.rds" = function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    r <- pkg_plan$new("crayon", lib = tempfile())
    r$resolve()
    r$solve()
  },

  "solution-igraph.rds" = function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    r <- pkg_plan$new("igraph", lib = tempfile())
    r$resolve()
    r$solve()
  }
)

fixture_dir <- function() {
  ## If run from R CMD check, it might give an error,
  ## so fall back to the current directory being tests/testthat
  tryCatch(
    file.path(
      rprojroot::find_package_root_file(),
      "tests", "testthat", "fixtures"
    ),
    error = function(e) "fixtures"
  )
}

get_fixture <- function(file) {
  file.path(fixture_dir(), file)
}

read_fixture <- function(file) {
  readRDS(get_fixture(file))
}

update_fixtures <- function(files = NULL) {
  if (is.null(files)) files <- names(fixtures)
  fdir <- fixture_dir()
  for (f in files) {
    output <- file.path(fdir, f)
    cat(output, sep = "\n")
    saveRDS(fixtures[[f]](), file = output, version = 2)
  }
}
