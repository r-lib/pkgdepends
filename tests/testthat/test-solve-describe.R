
test_that("conflicting dependencies", {
  pkgs <- make_fake_resolution(
    `aa` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cran::cc"))),
    `bb` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cc/cc"))),
    `cran::cc` = list(),
    `cc/cc` = list(extra = list(list(remotesha = "badcafe")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("aa", "cran::cc"))
  expect_equal(dsc$type, c("standard", "cran"))
  expect_equal(dsc$direct, c(TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK"))
  expect_equal(dsc$package, c("aa", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "conflict"))
  expect_snapshot(dsc)
})

test_that("conflicting dependencies downstream", {
  pkgs <- make_fake_resolution(
    `a0` = list(direct = TRUE, deps = list(make_fake_deps(Imports = "aa, bb"))),
    `aa` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cran::cc"))),
    `bb` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cc/cc"))),
    `cran::cc` = list(),
    `cc/cc` = list(extra = list(list(sha = "badcafe")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("a0", "bb", "cc/cc"))
  expect_equal(dsc$type, c("standard", "standard", "github"))
  expect_equal(dsc$direct, c(TRUE, TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK", "OK"))
  expect_equal(dsc$package, c("a0", "bb", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "dep-failed", "conflict"))
  expect_snapshot(dsc)
})

test_that("conflicting dependencies and installed packages", {

  gh <- list(
    users = list(
      "cran" = list(
        repos = list(
          dm = list(
            commits = list(
              list(
                sha = "96dac7a1cffea51709fa3c53c3424d4047ec279286583b8e1e46c701e98d0954",
                branch = "main",
                tag = "1.0.0",
                files = list(
                  DESCRIPTION = "Package: dm\nVersion: 1.0.0\nImports: tidyr\n",
                  NAMESPACE = ""
                )
              )
            )
          ),
          dplyr = list(
            commits = list(
              list(
                sha = "b259bcc4a82d28effdc59d3d51d4976b32ce29056ef9da48eeb9424e6eaffa16",
                branch = "main",
                tag = "1.0.9",
                files = list(
                  DESCRIPTION = "Package: dplyr\nVersion: 1.0.9\n"
                )
              )
            )
          )
        )
      )
    )
  )

  cran <- dcf("
    Package: tidyr
    Version: 1.0.0
    Imports: dplyr (>= 1.0.10)

    Package: dplyr
    Version: 1.1.1

    Package: foo
    Imports: bar

    Package: bar
  ")

  fake_gh <- webfakes::local_app_process(gh_app(gh))
  withr::local_envvar(R_PKG_GITHUB_API_URL = fake_gh$url())
  setup_fake_apps(cran_repo = cran)

  # -----------------------------------------------------------------------

  lib <- withr::local_tempdir()

  p <- new_pkg_installation_proposal(
    c("cran/dm@1.0.0", "cran/dplyr@1.0.9"),
    config = list(library = lib)
  )
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  expect_snapshot(p$get_solution()$failures)

  # -----------------------------------------------------------------------
  # fake an installed package

  dir.create(file.path(lib, "tidyr"))
  writeLines(c(
    "Package: tidyr",
    "Version: 1.0.0",
    "Imports: dplyr (>= 1.0.10)",
    "Repository: CRAN",
    # need this on older windows, because pak matches archs on that
    if (is_windows()) {
      paste0(
        "Built: R ", getRversion(), "; ",
        "i386+x86_64-w64-mingw32; ",
        "2023-02-06 08:15:41 UTC; ",
        "windows"
      )
    }
  ), file.path(lib, "tidyr", "DESCRIPTION"))
  p <- new_pkg_installation_proposal(
    c("cran/dm@1.0.0", "cran/dplyr@1.0.9"),
    config = list(library = lib)
  )
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  expect_snapshot(
    p$get_solution()$failures,
    transform = function(x) {
      sub("installed::.*tidyr", "installed::<path>/tidyr", x)
    }
  )

  # -----------------------------------------------------------------------
  # cannot install anything, accoridng to the solver...

  local <- withr::local_tempdir()
  writeLines(c(
    "Package: local123",
    "Version: 1.0.0",
    "Imports: dm, dplyr",
    "Remotes: cran/dm@1.0.0, cran/dplyr@1.0.9"
  ), file.path(local, "DESCRIPTION"))

  # First without installed packages
  p <- new_pkg_installation_proposal(
    paste0("local::", local),
    config = list(library = lib)
  )
  suppressMessages(p$resolve())
  suppressMessages(p$solve())
  expect_snapshot(
    p$get_solution()$failures,
    transform = function(x) {
      sub(paste0("local::.*", basename(local)), "local::<path>/<pkg>", x)
    }
  )

  # -----------------------------------------------------------------------
  # cannot install anything for some direct refs

  p2 <- new_pkg_installation_proposal(
    c(paste0("local::", local), "foo"),
    config = list(library = lib)
  )
  suppressMessages(p2$resolve())
  suppressMessages(p2$solve())
  expect_snapshot(
    p2$get_solution()$failures,
    transform = function(x) {
      sub(paste0("local::.*", basename(local)), "local::<path>/<pkg>", x)
    }
  )
})
