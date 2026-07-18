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

test_that("source_deps recovers LinkingTo dependencies", {
  plan <- data_frame(
    package = c("A", "B", "C", "D"),
    dependencies = list("C", character(), character(), character()),
    deps = list(
      data_frame(package = c("B", "C"), type = c("LinkingTo", "Imports")),
      data_frame(package = character(), type = character()),
      data_frame(package = character(), type = character()),
      data_frame(package = character(), type = character())
    ),
    dep_types = rep(list(pkg_dep_types_hard()), 4)
  )

  # LinkingTo (B) is added back, in addition to the known Imports (C)
  expect_equal(sort(source_deps(plan, 1L)), c("B", "C"))

  # falls back to the `dependencies` column if there is no `deps` column
  plan$deps <- NULL
  expect_equal(source_deps(plan, 1L), "C")
})

test_that("handle_install_needs_build requeues the package as source", {
  local_cli_config()
  # A is a binary depending on B (LinkingTo) and C (Imports), C depends on D.
  # As a binary, A's `dependencies` dropped the LinkingTo B.
  plan <- data_frame(
    package = c("A", "B", "C", "D"),
    version = "1.0.0",
    binary = TRUE,
    needscompilation = "no",
    dependencies = list("C", character(), "D", character()),
    deps = list(
      data_frame(package = c("B", "C"), type = c("LinkingTo", "Imports")),
      data_frame(package = character(), type = character()),
      data_frame(package = "D", type = "Imports"),
      data_frame(package = character(), type = character())
    ),
    dep_types = rep(list(pkg_dep_types_hard()), 4),
    build_done = TRUE,
    install_done = c(FALSE, FALSE, TRUE, TRUE),
    worker_id = NA_character_,
    deps_left = rep(list(character()), 4)
  )
  state <- list(plan = plan, workers = list(), config = list())
  worker <- list(
    task = list(args = list(pkgidx = 1L)),
    result = structure(
      list(needscompilation = "yes"),
      class = "install_needs_build"
    )
  )

  state <- suppressMessages(handle_install_needs_build(state, worker))

  # A is now a source package that still needs to be built
  expect_false(state$plan$binary[[1]])
  expect_false(state$plan$build_done[[1]])
  expect_true(is.na(state$plan$worker_id[[1]]))
  expect_equal(state$plan$needscompilation[[1]], "yes")

  # Its dependency closure includes the recovered LinkingTo dep, and it now
  # waits only for the not-yet-installed ones (B; C and D are installed).
  expect_equal(sort(state$plan$dependencies[[1]]), c("B", "C", "D"))
  expect_equal(state$plan$deps_left[[1]], "B")
})

test_that("handle_install_needs_build errors on missing LinkingTo dep", {
  local_cli_config()
  # A LinkingTo X, but X is not part of the plan (dropped, because A was a
  # binary). We cannot build A from source without X.
  plan <- data_frame(
    package = c("A", "B"),
    version = "1.0.0",
    binary = TRUE,
    needscompilation = "no",
    dependencies = list("B", character()),
    deps = list(
      data_frame(package = c("X", "B"), type = c("LinkingTo", "Imports")),
      data_frame(package = character(), type = character())
    ),
    dep_types = rep(list(pkg_dep_types_hard()), 2),
    build_done = TRUE,
    install_done = c(FALSE, TRUE),
    worker_id = NA_character_,
    deps_left = rep(list(character()), 2)
  )
  state <- list(plan = plan, workers = list(), config = list())
  worker <- list(
    task = list(args = list(pkgidx = 1L)),
    result = structure(
      list(needscompilation = "yes"),
      class = "install_needs_build"
    )
  )

  expect_snapshot(
    error = TRUE,
    handle_install_needs_build(state, worker)
  )
})

test_that("lockfile_deps reconstructs the deps data frame", {
  # empty / missing input yields an empty deps frame
  expect_equal(lockfile_deps(NULL), make_null_deps())
  expect_equal(lockfile_deps(list()), make_null_deps())

  deps <- list(
    list(
      ref = "B",
      type = "Imports",
      package = "B",
      op = "",
      version = ""
    ),
    list(
      ref = "C",
      type = "LinkingTo",
      package = "C",
      op = ">=",
      version = "1.0.0"
    )
  )
  expect_equal(
    lockfile_deps(deps),
    data_frame(
      ref = c("B", "C"),
      type = c("Imports", "LinkingTo"),
      package = c("B", "C"),
      op = c("", ">="),
      version = c("", "1.0.0")
    )
  )
})

test_that("ignore-build-errors parameter", {
  setup_fake_apps()
  local_cli_config()
  dir.create(tmplib <- tempfile())
  on.exit(rimraf(tmplib), add = TRUE)
  pkgdir1 <- test_path("fixtures", "packages", "badbuild")
  pkgdir2 <- test_path("fixtures", "packages", "goodbuild")
  inst <- new_pkg_installation_proposal(
    paste0("local::", c(pkgdir1, pkgdir2), "?nocache"),
    config = list(library = tmplib, platforms = "source")
  )
  expect_snapshot(
    {
      suppressMessages(inst$solve())
      suppressMessages(inst$download())
      inst$install()
    },
    error = TRUE
  )

  inst <- new_pkg_installation_proposal(
    paste0("local::", c(pkgdir1, pkgdir2), "?nocache&ignore-build-errors"),
    config = list(library = tmplib, platforms = "source")
  )
  expect_snapshot({
    suppressMessages(inst$solve())
    suppressMessages(inst$download())
    inst$install()
  })
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
    built = TRUE,
    platform = current_r_platform()
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
    built = TRUE,
    platform = current_r_platform()
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
