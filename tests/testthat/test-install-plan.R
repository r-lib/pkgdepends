
test_that("can package a tree", {

  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  file.copy(test_path("fixtures", "foo"), tmp, recursive = TRUE)

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data_frame(
    ref = paste0("local::./foo"),
    type = "local",
    direct = TRUE,
    status = "OK",
    package = "foo",
    version = "0.0.0.9000",
    binary = FALSE,
    packaged = FALSE,
    dependencies = list(character()),
    file = tmp,
    needscompilation = FALSE,
    install_args = ""
  )

  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
})

test_that("can package a compressed tree", {

  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  pkgzip <- file.path(tmp, "foo-t.zip")
  zip::zipr(pkgzip, test_path("fixtures", "foo"))

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data.frame(
    stringsAsFactors = FALSE,
    ref = paste0("local::./foo"),
    type = "local",
    direct = TRUE,
    status = "OK",
    package = "foo",
    version = "0.0.0.9000",
    binary = FALSE,
    packaged = FALSE,
    dependencies = I(list(character())),
    file = pkgzip,
    vignettes = FALSE,
    needscompilation = FALSE,
    metadata = I(list(character()))
  )

  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
})


test_that("can package a source package", {

  skip_on_cran()
  local_cli_config()

  tmp <- withr::local_tempdir()
  pkg <- source_test_package("foo")

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  plan <- data.frame(
    stringsAsFactors = FALSE,
    ref = paste0("local::./foo"),
    type = "local",
    direct = TRUE,
    status = "OK",
    package = "foo",
    version = "0.0.0.9000",
    binary = FALSE,
    dependencies = I(list(character())),
    file = pkg,
    vignettes = FALSE,
    needscompilation = FALSE,
    metadata = I(list(character()))
  )

  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_true(file.exists(file.path(lib, "foo")))
  expect_true(file.exists(file.path(lib, "foo", "DESCRIPTION")))
  expect_true(file.exists(file.path(lib, "foo", "NAMESPACE")))
  expect_true(file.exists(file.path(lib, "foo", "installed-file")))
})

test_that("add_recursive_dependencies", {
  plan <- data_frame(
    package = c("p1", "p2", "p3"),
    type = "cran",
    binary = TRUE,
    dependencies = list("p2", "p3", character()),
    file = NA_character_,
    needscompilation = FALSE
  )

  expect_equal(add_recursive_dependencies(plan), plan)

  plan$binary <- FALSE
  expect_equal(add_recursive_dependencies(plan), plan)

  plan$binary <- c(FALSE, TRUE, FALSE)
  expect_snapshot(add_recursive_dependencies(plan))
  expect_snapshot(add_recursive_dependencies(plan)$dependencies)
})

test_that("install package from GH", {
  setup_fake_apps()
  setup_fake_gh_app()
  pkgcache::pkg_cache_delete_files()

  lib <- withr::local_tempdir()
  config <- list(library = lib)
  pkg <- new_pkg_installation_proposal("r-lib/crayon", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "crayon")))

  # check cache state, must have a tree, a source and a binary package
  check_cache <- function() {
    cache <- pkgcache::pkg_cache_list()
    expect_equal(nrow(cache), 3L)
    expect_true(any(cache$package == "crayon" & !cache$built))
    expect_true(any(
      cache$package == "crayon" & cache$built & cache$platform == "source"
    ))
    expect_true(any(
      cache$package == "crayon" & cache$built & cache$platform != "source"
    ))
  }
  check_cache()

  # install from cache, binary is selected
  remove.packages("crayon", lib = lib)
  pkg <- new_pkg_installation_proposal("r-lib/crayon", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  expect_equal(
    pkg$get_downloads()$download_status,
    paste("Had", current_r_platform())
  )
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "crayon")))

  # cache is updated with the binary
  check_cache()

  # install from cache, no binary, source package is selected
  remove.packages("crayon", lib = lib)
  pkgcache::pkg_cache_delete_files(
    built = TRUE, platform = current_r_platform()
  )
  pkg <- new_pkg_installation_proposal("r-lib/crayon", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  expect_equal(pkg$get_downloads()$download_status, "Had")
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "crayon")))

  # install from cache, no binary, no source, tree is selected
  remove.packages("crayon", lib = lib)
  pkgcache::pkg_cache_delete_files(built = TRUE)
  pkg <- new_pkg_installation_proposal("r-lib/crayon", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  expect_equal(pkg$get_downloads()$download_status, "Had")
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "crayon")))

  # cache is updated with the source and binary
  check_cache()
})

test_that("install package from GH, in subdir", {
  setup_fake_apps()
  setup_fake_gh_app()
  pkgcache::pkg_cache_delete_files()

  lib <- withr::local_tempdir()
  config <- list(library = lib)
  pkg <- new_pkg_installation_proposal("wesm/feather/R", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # check cache state, must have a tree, a source and a binary package
  check_cache <- function() {
    cache <- pkgcache::pkg_cache_list()
    expect_equal(nrow(cache), 3L)
    expect_true(any(cache$package == "feather" & !cache$built))
    expect_true(any(
      cache$package == "feather" & cache$built & cache$platform == "source"
    ))
    expect_true(any(
      cache$package == "feather" & cache$built & cache$platform != "source"
    ))
  }
  check_cache()

  # install from cache, binary is selected
  remove.packages("feather", lib = lib)
  pkg <- new_pkg_installation_proposal("wesm/feather/R", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  expect_equal(
    pkg$get_downloads()$download_status,
    paste("Had", current_r_platform())
  )
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # cache is updated with the binary
  check_cache()

  # install from cache, no binary, source package is selected
  remove.packages("feather", lib = lib)
  pkgcache::pkg_cache_delete_files(
    built = TRUE, platform = current_r_platform()
  )
  pkg <- new_pkg_installation_proposal("wesm/feather/R", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  expect_equal(pkg$get_downloads()$download_status, "Had")
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # install from cache, no binary, no source, tree is selected
  remove.packages("feather", lib = lib)
  pkgcache::pkg_cache_delete_files(built = TRUE)
  pkg <- new_pkg_installation_proposal("wesm/feather/R", config = config)
  suppressMessages(pkg$solve())
  suppressMessages(pkg$download())
  expect_equal(pkg$get_downloads()$download_status, "Had")
  suppressMessages(pkg$install())
  expect_true(file.exists(file.path(lib, "feather")))

  # cache is updated with the source and binary
  check_cache()
})
