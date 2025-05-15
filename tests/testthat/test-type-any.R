test_that("parse_remote_any", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()
  lib <- withr::local_tempdir()

  # create a lockfile first
  plan <- suppressMessages(new_pkg_installation_proposal(
    "any::pkg3",
    config = list(library = lib)
  ))
  suppressMessages(plan$resolve())
  plan$solve()
  lockfile <- tempfile(fileext = ".lock")
  on.exit(unlink(lockfile), add = TRUE)
  plan$create_lockfile(lockfile)

  plan <- suppressMessages(new_pkg_installation_proposal(
    "any::pkg3",
    config = list(library = lib)
  ))
  suppressMessages(plan$resolve())
  plan$solve()
  expect_snapshot(plan$draw(), transform = transform_bytes)
  suppressMessages(plan$download())
  suppressMessages(plan$install())

  # do it again, should keep installed package
  plan <- suppressMessages(new_pkg_installation_proposal(
    "any::pkg3",
    config = list(library = lib)
  ))
  suppressMessages(plan$resolve())
  plan$solve()
  expect_snapshot(plan$draw(), transform = transform_bytes)
  suppressMessages(plan$download())
  suppressMessages(plan$install())

  # Now check the lockfile, it should not install anything
  plan <- pkgdepends::new_pkg_installation_plan(
    lockfile,
    config = list(library = lib)
  )

  suppressMessages(plan$update())
  suppressMessages(plan$download())
  suppressMessages(plan$install())
})

test_that("dep_types", {
  setup_fake_apps()
  pkgcache::pkg_cache_delete_files()
  lib <- withr::local_tempdir()

  plan <- suppressMessages(new_pkg_installation_proposal(
    c("any::pkg3", "pkg3"),
    config = list(library = lib, dependencies = TRUE)
  ))
  suppressMessages(plan$resolve())
  plan$solve()
  res <- plan$get_resolution()
  res <- res[order(res$ref), ]
  expect_snapshot(as.list(res[c("ref", "dep_types")]))
  suppressMessages(plan$download())
  suppressMessages(plan$install())

  plan <- suppressMessages(new_pkg_installation_proposal(
    c("any::pkg3", "pkg3"),
    config = list(library = lib, dependencies = TRUE)
  ))
  suppressMessages(plan$resolve())
  plan$solve()

  res <- plan$get_resolution()
  res <- res[order(res$ref), ]
  res$ref <- sub("installed::.*pkg", "installed::<path>/pkg", res$ref)
  expect_snapshot(as.list(res[c("ref", "dep_types")]))
})
