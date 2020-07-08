library(testthat)
library(pkgdepends)

test_check(
  "pkgdepends",
  reporter = SummaryReporter$new(max_reports = 1000)
)
