library(testthat)
library(pkgdepends)

Sys.setenv(R_USER_CACHE_DIR = tempdir())
test_check("pkgdepends")
