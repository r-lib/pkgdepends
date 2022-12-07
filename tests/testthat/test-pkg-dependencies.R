
test_that("new_pkg_deps", {
  pkgcache::pkg_cache_delete_files()
  setup_fake_apps()

  deps <- new_pkg_deps("pkg3", config = list(library = tempfile()))
  expect_snapshot(deps)
  expect_snapshot(deps$get_refs())
  expect_snapshot(sort(deps$get_config()$list()))

  suppressMessages(deps$resolve())
  expect_snapshot(deps)

  suppressMessages(deps$solve())
  expect_snapshot(deps)

  expect_snapshot(
    deps$draw(),
    transform = transform_bytes
  )

  expect_snapshot(
    deps$get_solution()
  )
})

test_that("async", {
  setup_fake_apps()

  deps <- new_pkg_deps("pkg3", config = list(library = tempfile()))
  expect_snapshot(
    synchronize(
      deps$async_resolve()$
      then(function() "done")
    )
  )
  expect_snapshot(
    deps$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
  )
})

test_that("solve policy", {
  setup_fake_apps()

  deps <- new_pkg_deps("pkg3", config = list(library = tempfile()))
  deps$get_solve_policy()
  deps$set_solve_policy("upgrade")
  suppressMessages(deps$solve())
  expect_snapshot(deps)
})

test_that("errors", {
  setup_fake_apps()

  deps <- new_pkg_deps("needsfuturama", config = list(library = tempfile()))
  expect_silent(deps$solve())
  expect_snapshot(
    error = TRUE,
    deps$stop_for_solution_error()
  )
})
