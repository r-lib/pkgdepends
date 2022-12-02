
test_that("local dep needs name", {
  setup_fake_apps()

  withr::local_dir(tmp <- withr::local_tempdir())

  mkdirp("pkg1")
  file.create("pkg1/NAMESPACE")
  writeLines(c(
    "Package: pkg1",
    "Version: 1.0.0",
    "License: MIT",
    "Imports: pkg2",
    "Remotes: local::./pkg2"
  ), "pkg1/DESCRIPTION")

  mkdirp("pkg2")
  file.create("pkg2/NAMESPACE")
  writeLines(c(
    "Package: pkg2",
    "Version: 1.0.0",
    "License: MIT"
  ), "pkg2/DESCRIPTION")

  mkdirp("lib")
  config <- list(library = file.path(tmp, "lib"))
  p <- new_pkg_installation_proposal("./pkg1", config = config)
  p$solve()
  expect_snapshot(
    error = TRUE,
    p$stop_for_solution_error(),
    transform = transform_no_srcref
  )

  desc::desc_set(Remotes = "pkg2=local::./pkg2", file = "pkg1")
  p <- new_pkg_installation_proposal("./pkg1", config = config)
  p$solve()
  expect_snapshot(p$get_solution())
})

test_that("url dep needs name", {
  setup_fake_apps()

  withr::local_dir(tmp <- withr::local_tempdir())

  mkdirp("pkg2")
  file.create("pkg2/NAMESPACE")

  url <- paste0(
    fake_cran$url(),
    "/src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz"
  )
  writeLines(c(
    "Package: pkg2",
    "Version: 1.0.0",
    "License: MIT",
    "Imports: pkg1",
    paste0("Remotes: url::", url)
  ), "pkg2/DESCRIPTION")

  mkdirp("lib")
  config <- list(library = file.path(tmp, "lib"))
  p <- new_pkg_installation_proposal("./pkg2", config = config)
  p$solve()
  expect_snapshot(
    error = TRUE,
    p$stop_for_solution_error(),
    transform = function(x) transform_no_srcref(transform_local_port(x))
  )

  desc::desc_set(Remotes = paste0("pkg1=url::", url), file = "pkg2")
  p <- new_pkg_installation_proposal("./pkg2", config = config)
  p$solve()
  expect_snapshot(
    p$get_solution(),
    transform = transform_local_port
  )
})
