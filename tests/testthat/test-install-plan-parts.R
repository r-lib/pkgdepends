
test_that("make_start_state", {
  plan <- readRDS(test_path("fixtures/sample_plan.rds"))
  state <- make_start_state(plan, list(foo = "bar"))

  expect_equal(names(state), c("plan", "workers", "config"))
  xcols <- c(
    "build_done", "build_time", "build_error", "build_stdout",
    "install_done", "install_time", "install_error",
    "install_stdout")
  expect_true(all(xcols %in% colnames(state$plan)))
  eq_cols <- setdiff(colnames(plan), "deps_left")
  expect_identical(
    as.data.frame(plan[, eq_cols]),
    as.data.frame(state$plan[, eq_cols])
  )
})

test_that("are_we_done", {
  plan <- readRDS(test_path("fixtures/sample_plan.rds"))
  state <- make_start_state(plan, list(foo = "bar"))
  expect_false(are_we_done(state))

  state$plan$install_done <- TRUE
  state$plan$install_done[1] <- FALSE
  expect_false(are_we_done(state))

  state$plan$install_done[1] <- TRUE
  expect_true(are_we_done(state))
})

test_that("poll_workers", {
  state <- list(workers = list())
  expect_equal(poll_workers(state), logical())

  skip_on_os("windows")

  ## These might fail, but that does not matter much here
  p1 <- processx::process$new("true", stdout = "|", stderr = "2>&1")
  p2 <- processx::process$new("true", stdout = "|", stderr = "2>&1")

  state <- list(workers = list(list(process = p1)))
  expect_equal(poll_workers(state), TRUE)

  state <- list(workers = c(state$workers, list(list(process = p2))))
  expect_true(any(poll_workers(state)))

  opts <- callr::r_process_options(func = function() Sys.sleep(5))
  p3 <- callr::r_process$new(opts)
  on.exit(p3$kill(), add = TRUE)
  state <- list(workers = c(state$workers, list(list(process = p3))))
  p <- poll_workers(state)
  expect_true(any(p))
  expect_false(p[3])
  p3$kill()
})

test_that("handle_event, process still running", {
  ## If just output, but the process is still running, then collect
  ## stdout and stderr
  plan <- readRDS(test_path("fixtures/sample_plan.rds"))
  state <- make_start_state(plan, list(num_workers = 2))

  mockery::stub(
    start_task_build, "make_build_process",
    make_dummy_worker_process())

  ## Run a dummy worker that runs for 10s, writes to stdout & stderr
  expect_snapshot(
    state <- start_task_build(state, task("build", pkgidx = 1))
  )
  proc <- state$workers[[1]]$process
  on.exit(proc$kill(), add = TRUE)

  for (i in 1:2) {
    proc$poll_io(-1)
    state <- handle_event(state, 1)
    expect_false(is.null(state$workers[[1]]))
    ## We cannot be sure that both stdout and stderr are already there,
    ## but one of them must be
    out <- paste(state$workers[[1]]$stdout, collapse = "")
    err <- paste(state$workers[[1]]$stderr, collapse = "")
    expect_true(grepl("^out ", out) || grepl("^err ", err))
    expect_true(proc$is_alive())
    expect_false(is.na(state$plan$worker_id[1]))
  }

  proc$kill()
})

test_that("handle_event, build process finished", {
  local_cli_config()
  plan <- readRDS(test_path("fixtures/sample_plan.rds"))
  state <- make_start_state(plan, list(foo = "bar"))
  state$plan$build_done[1] <- FALSE

  mockery::stub(
    start_task_build, "make_build_process",
    make_dummy_worker_process(n_iter = 2, sleep = 0))

  expect_snapshot(
    state <- start_task_build(state, task("build", pkgidx = 1))
  )

  proc <- state$workers[[1]]$process
  on.exit(proc$kill(), add = TRUE)

  expect_snapshot(repeat {
    events <- poll_workers(state)
    state <- handle_events(state, events)
    if (all(state$plan$build_done)) break;
  })

  expect_false(proc$is_alive())
  expect_false(state$plan$build_error[[1]])
  expect_equal(state$plan$build_stdout[[1]], c("out 1", "err 1", "out 2", "err 2"))
  expect_identical(state$plan$worker_id[[1]], NA_character_)
  expect_equal(length(state$workers), 0)
})

test_that("handle event, build process finished, but failed", {
  local_cli_config()
  plan <- readRDS("fixtures/sample_plan.rds")
  expect_snapshot(state <- make_start_state(plan, list(foo = "bar")))
  state$plan$build_done[1] <- FALSE

  mockery::stub(
    start_task_install, "make_install_process",
    make_dummy_worker_process(n_iter = 2, sleep = 0, status = 1))

  expect_snapshot(
    state <- start_task_install(state, task("install", pkgidx = 1))
  )
  proc <- state$workers[[1]]$process
  on.exit(proc$kill(), add = TRUE)

  expect_snapshot(error = TRUE, repeat {
      events <- poll_workers(state)
      state <- handle_events(state, events)
      if (all(state$plan$build_done)) break;
    }
  )

})

test_that("handle_event, install process finished", {
  plan <- readRDS("fixtures/sample_plan.rds")
  state <- make_start_state(plan, list(foo = "bar"))

  mockery::stub(
    start_task_install, "make_install_process",
    make_dummy_worker_process(n_iter = 2, sleep = 0))

  state <- start_task_install(state, task("install", pkgidx = 1))
  proc <- state$workers[[1]]$process
  on.exit(proc$kill(), add = TRUE)

  done <- FALSE
  expect_snapshot(repeat {
    events <- poll_workers(state)
    state <- handle_events(state, events)
    if (done) break
    if (!proc$is_alive()) done <- TRUE
  })

  expect_false(proc$is_alive())
  expect_false(state$plan$install_error[[1]])
  expect_equal(state$plan$install_stdout[[1]], c("out 1", "err 1", "out 2", "err 2"))
  expect_identical(state$plan$worker_id[[1]], NA_character_)
  expect_equal(length(state$workers), 0)
})

test_that("handle event, install process finished, but failed", {
  plan <- readRDS("fixtures/sample_plan.rds")
  state <- make_start_state(plan, list(foo = "bar"))

  mockery::stub(
    start_task_install, "make_install_process",
    make_dummy_worker_process(n_iter = 2, sleep = 0, status = 1))

  state <- start_task_install(state, task("install", pkgidx = 1))
  proc <- state$workers[[1]]$process
  on.exit(proc$kill(), add = TRUE)

  expect_snapshot(error = TRUE, {
    done <- FALSE
    repeat {
      events <- poll_workers(state)
      state <- handle_events(state, events)
      if (done) break
      if (!proc$is_alive()) done <- TRUE
    }
  })
})

test_that("select_next_task", {
  plan <- readRDS("fixtures/sample_plan.rds")
  state <- make_start_state(plan, list(num_workers = 2))

  ## If no more workers are available
  state$workers <- list(list("dummy1"), list("dummy2"))
  expect_equal(select_next_task(state), task("idle"))

  ## An ongoing install task is not selected again
  state <- make_start_state(plan, list(num_workers = 2))
  state$plan$worker_id[-nrow(state$plan)] <- 42
  expect_equal(
    select_next_task(state),
    task("install", pkgidx = nrow(state$plan)))

  ## An ongoing build task is not selected again
  state <- make_start_state(plan, list(num_workers = 2))
  state$plan$build_done <- FALSE
  state$plan$deps_left[] <- rep_list(nrow(state$plan), character())
  state$plan$worker_id[-nrow(state$plan)] <- 42
  expect_equal(
    select_next_task(state),
    task("build", pkgidx = nrow(state$plan)))

  ## Source is preferred over binary
  state <- make_start_state(plan, list(num_workers = 2))
  state$plan$build_done[nrow(state$plan)] <- FALSE
  state$plan$deps_left[] <- rep_list(nrow(state$plan), character())
  expect_equal(
    select_next_task(state),
    task("build", pkgidx = nrow(state$plan)))

  ## Source is selected only if dependencies are done
  state <- make_start_state(plan, list(num_workers = 2))
  state$plan$build_done <- FALSE
  state$plan$deps_left[] <- rep_list(nrow(state$plan), "foobar")
  state$plan$deps_left[[nrow(state$plan)]] <- character()
  expect_equal(
    select_next_task(state),
    task("build", pkgidx = nrow(state$plan)))

  ## Binary is selected irrespectively of dependencies
  state <- make_start_state(plan, list(num_workers = 2))
  state$plan$deps_left[] <- rep_list(nrow(state$plan), "foobar")
  expect_equal(
    select_next_task(state),
    task("install", pkgidx = 1L))

  ## We cannot select anything, because of the dependencies
  state <- make_start_state(plan, list(num_workers = 2))
  state$plan$build_done <- FALSE
  state$plan$worker_id[1] <- 1
  state$plan$deps_left[] <- rep_list(nrow(state$plan), "foobar")
  expect_equal(
    select_next_task(state),
    task("idle"))
})

test_that("start_task", {
  expect_error(
    start_task(list(), task("foobar")),
    "Unknown task"
  )
})

test_that("stop_task", {
  expect_error(
    stop_task(list(), list(task = task("foobar"))),
    "Unknown task"
  )
})

test_that("get_worker_id", {
  expect_true(get_worker_id() != get_worker_id())
})

test_that("kill_all_processes", {

  skip_on_os("windows")

  p1 <- processx::process$new("true", stdout = "|")
  on.exit(p1$kill(), add = TRUE)
  p2 <- processx::process$new("true", stdout = "|")
  on.exit(p2$kill(), add = TRUE)
  opts <- callr::r_process_options(func = function() Sys.sleep(5))
  p3 <- callr::r_process$new(opts)
  on.exit(p3$kill(), add = TRUE)

  state <- list(workers = list(
    list(process = p1),
    list(process = p2),
    list(process = p3)
  ))

  kill_all_processes(state)

  expect_false(p1$is_alive())
  expect_false(p2$is_alive())
  expect_false(p3$is_alive())

  p1$kill()
  p2$kill()
  p3$kill()
})

test_that("kill_all_processes that catch/ignore SIGINT", {

  skip_on_cran()
  skip_on_os("windows")
  if (Sys.which("bash") == "") skip("Needs 'bash'")

  sh <- "trap '>&2 echo \"Hold on\"' INT
    for ((n=5; n; n--))
    do
      echo going
      sleep 1
    done"

  px <- processx::process$new("bash", c("-c", sh), stdout = "|", stderr = "|")
  expect_true(px$is_alive())

  state <- list(workers = list(list(process = px)))

  ## Need to wait until the shell starts and traps SIGINT
  px$poll_io(2000)

  tic <- Sys.time()
  kill_all_processes(state)
  limit <- Sys.time() + as.difftime(3, units = "secs")
  while (px$is_alive() && Sys.time() < limit) Sys.sleep(0.05)
  expect_true(Sys.time() < limit)
  expect_true(Sys.time() - tic > as.difftime(0.2, units = "secs"))
  expect_false(px$is_alive())

  ## We can't get the output of the signal handler, because SIGKILL
  ## does not ensure emptying the buffers....

  px$kill()
})

test_that("deadlock detection", {
  plan <- data_frame(
    package = c("p1", "p2", "p3"),
    type = "cran",
    binary = FALSE,
    dependencies = list("p2", "p3", "p1"),
    file = NA_character_,
    needscompilation = FALSE
  )

  expect_snapshot(
    error = TRUE,
    install_package_plan(plan, lib = tempfile())
  )
})

test_that("make_build_process", {
  tmp <- withr::local_tempdir()
  mkdirp(file.path(tmp, "subdir"))
  file.copy(
    test_path("fixtures", "foo"),
    file.path(tmp, "subdir"),
    recursive = TRUE
  )

  p <- make_build_process(
    tmp,
    "foo",
    tempdir(),
    .libPaths(),
    vignettes = FALSE,
    needscompilation = TRUE,
    binary = FALSE,
    cmd_args = character()
  )

  p$wait(5000)
  p$kill()
  blt <- p$get_built_file()
  expect_equal(basename(blt), "foo_0.0.0.9000.tar.gz")
  expect_true(file.exists(blt))

  p <- make_build_process(
    file.path(tmp, "subdir"),
    "foo", tempdir(),
    .libPaths(),
    vignettes = FALSE,
    needscompilation = TRUE,
    binary = FALSE,
    cmd_args = character()
  )

  p$wait(5000)
  p$kill()
  blt <- p$get_built_file()
  expect_equal(basename(blt), "foo_0.0.0.9000.tar.gz")
  expect_true(file.exists(blt))
})

test_that("install_args are passed", {
  withr::local_envvar(PKG_OMIT_TIMES = "true")

  pkg <- source_test_package("foo")
  plan <- data_frame(
    type = "local",
    binary = FALSE,
    dependencies = list(character()),
    file = pkg,
    needscompilation = TRUE,
    package = "foo",
    install_args = "--no-inst"
  )

  lib <- withr::local_tempdir()
  expect_snapshot(install_package_plan(plan, lib = lib))

  expect_false(file.exists(file.path(lib, "foo", "installed-file")))
})

test_that("built package is added to the cache", {

})

test_that("installed_note", {
  expect_snapshot({
    installed_note(list(type = "cran"))
    installed_note(list(type = "bioc"))
    installed_note(list(type = "standard"))
    installed_note(list(type = "local"))
    installed_note(list(
      type = "github",
      metadata = list(list(
        RemoteUsername = "r-lib",
        RemoteRepo = "pak",
        RemoteSha = "5a4da54df42528545af8a64e83112be21273907c6dfa0f31a0982ca88db6527d"
      ))
    ))
  })
})
