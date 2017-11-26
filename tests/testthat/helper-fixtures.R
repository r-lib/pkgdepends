
fixtures <- list(

  "resolution-simple.rds" = function() {
    r <- remotes$new("pkgconfig", lib = tempfile())
    r$resolve()
  },

  "resolution-installed.rds" = function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    install.packages("pkgconfig", lib = tmp)
    r <- remotes$new("pkgconfig", lib = tmp)
    r$resolve()
  },

  "resolution-gh-vs-cran.rds" = function() {
    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    r <- remotes$new(
      c("cran::pkgconfig", "github::r-lib/pkgconfig"),
      lib = tmp)
    r$resolve()
  }
)

fixture_dir <- function() {
  file.path(
    rprojroot::find_package_root_file(),
    "tests", "testthat", "fixtures"
  )
}

read_fixture <- function(file) {
  readRDS(file.path(fixture_dir(), file))
}

update_fixtures <- function(files = NULL) {
  if (is.null(files)) files <- names(fixtures)
  fdir <- fixture_dir()
  for (f in files) {
    output <- file.path(fdir, f)
    cat(output, sep = "\n")
    saveRDS(fixtures[[f]](), file = output)
  }
}
