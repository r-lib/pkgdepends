#' Rebuild fixtures
#'
#' @examples
#' testthat::test_dir(testthat::test_path("fixtures"))
#'

test_that("Rebuilding Git Packfile Fixtures", {
  fixture_updated <- function(msg) {
    stop(errorCondition(msg, class = "error_fixture_updated"))
  }

  write_packfile <- function(packfile) {
    function(x, ...) {
      path <- file.path(
        find_package_root(),
        "tests",
        "testthat",
        "fixtures",
        packfile
      )
      writeBin(x, path)
      fixture_updated(sprintf("Updated fixture: '%s'", path))
    }
  }

  url <- "https://github.com/dgkf/test-pkgdepends-git-remote-submod-req.git"
  expect_error(class = "error_fixture_updated", with_mocked_bindings(
    git_unpack = write_packfile("git-test-submod-req.pack"),
    git_download_repo(url)
  ))

  url <- "https://github.com/dgkf/test-pkgdepends-git-remote-submod-opt.git"
  expect_error(class = "error_fixture_updated", with_mocked_bindings(
    git_unpack = write_packfile("git-test-submod-opt.pack"),
    git_download_repo(url)
  ))
})
