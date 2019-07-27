
#' Perform a package installation plan, as created by pkgdepends
#'
#' @param plan Package plan object, returned by pkgdepends
#' @param lib Library directory to install to.
#' @param num_workers Number of worker processes to use.
#' @return Information about the installation process.
#'
#' @importFrom callr poll
#' @export

install_package_plan <- function(plan, lib = .libPaths()[[1]],
                                 num_workers = 1) {

  start <- Sys.time()

  required_columns <- c(
    "type", "binary", "dependencies", "file", "vignettes",
    "needscompilation", "metadata", "package")
  stopifnot(
    inherits(plan, "data.frame"),
    all(required_columns %in% colnames(plan)),
    is_string(lib),
    is_count(num_workers, min = 1L)
  )

  if (! "packaged" %in% colnames(plan)) plan$packaged <- TRUE

  config <- list(lib = lib, num_workers = num_workers)
  state <- make_start_state(plan, config)
  state$progress <- create_progress_bar(state)
  on.exit(done_progress_bar(state), add =  TRUE)

  withCallingHandlers({

    ## Initialise one task for each worker
    for (i in seq_len(state$config$num_workers)) {
      task <- select_next_task(state)
      state <- start_task(state, task)
    }

    repeat {
      if (are_we_done(state)) break;
      update_progress_bar(state)

      events <- poll_workers(state)
      state <- handle_events(state, events)
      task  <- select_next_task(state)
      state <- start_task(state, task)
    }
  }, error = function(e) kill_all_processes(state))

  create_install_result(state)
}

make_start_state <- function(plan, config) {

  ## We store the data about build and installation here
  install_cols <- data.frame(
    stringsAsFactors = FALSE,
    package_done = plan$packaged,
    package_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    package_error = I(rep_list(nrow(plan), list())),
    package_stdout = I(rep_list(nrow(plan), character())),
    package_stderr = I(rep_list(nrow(plan), character())),
    build_done = (plan$type %in% c("deps", "installed")) | plan$binary,
    build_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    build_error = I(rep_list(nrow(plan), list())),
    build_stdout = I(rep_list(nrow(plan), character())),
    build_stderr = I(rep_list(nrow(plan), character())),
    install_done = plan$type %in% c("deps", "installed"),
    install_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    install_error = I(rep_list(nrow(plan), list())),
    install_stdout = I(rep_list(nrow(plan), character())),
    install_stderr = I(rep_list(nrow(plan), character())),
    worker_id = NA_character_
  )
  plan <- cbind(plan, install_cols)

  installed <- plan$package[plan$install_done]
  plan$deps_left <- lapply(plan$dependencies, setdiff, installed)

  list(
    plan = plan,
    workers = list(),
    config = config)
}

are_we_done <- function(state) {
  all(state$plan$install_done)
}

#' @importFrom callr poll

poll_workers <- function(state) {
  if (length(state$workers)) {
    timeout <- get_timeout(state)
    procs <- lapply(state$workers, "[[", "process")
    res <- poll(procs, ms = timeout)
    map_lgl(res, function(x) "ready" %in% x)

  } else {
    logical()
  }
}

get_timeout <- function(state) 100

handle_events <- function(state, events) {
  for (i in which(events)) state <- handle_event(state, i)
  state$workers <- drop_nulls(state$workers)
  state
}

handle_event <- function(state, evidx) {
  proc <- state$workers[[evidx]]$process

  ## Read out stdout and stderr. If process is done, then read out all
  if (proc$is_alive()) {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_output(n = 10000))
    state$workers[[evidx]]$stderr <-
      c(state$workers[[evidx]]$stderr, err <- proc$read_error(n = 10000))
  } else {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_all_output())
    state$workers[[evidx]]$stderr <-
      c(state$workers[[evidx]]$stderr, err <- proc$read_all_error())
  }

  ## If there is still output, then wait a bit more
  if (proc$is_alive() ||
      proc$is_incomplete_output() || proc$is_incomplete_error()) {
    return(state)
  }

  ## Otherwise we are done. Remove worker
  worker <- state$workers[[evidx]]
  state$workers[evidx] <- list(NULL)

  ## Post-process, this will throw on error
  if (is.function(proc$get_result)) proc$get_result()

  ## Cut stdout and stderr to lines
  worker$stdout <- cut_into_lines(worker$stdout)
  worker$stderr <- cut_into_lines(worker$stderr)

  ## Record what was done
  stop_task(state, worker)
}

select_next_task <- function(state) {

  ## Cannot run more workers?
  if (length(state$workers) >= state$config$num_workers) {
    return(task("idle"))
  }

  ## Can we select a package tree to build into a source package? Do that.
  can_package <- which(
    ! state$plan$package_done &
    map_int(state$plan$deps_left, length) == 0 &
    is.na(state$plan$worker_id))

  if (any(can_package)) {
    pkgidx <- can_package[1]
    return(task("package", pkgidx = pkgidx, phase = "uncompress"))
  }

  ## Can we select a source package build? Do that.
  can_build <- which(
    ! state$plan$build_done &
    map_int(state$plan$deps_left, length) == 0 &
    is.na(state$plan$worker_id))

  if (any(can_build)) {
    pkgidx <- can_build[1]
    return(task("build", pkgidx = pkgidx))
  }

  ## TODO: can we select a binary that is depended on by a source package?

  ## Otherwise select a binary if there is one
  can_install <- which(
    state$plan$build_done &
    ! state$plan$install_done &
    is.na(state$plan$worker_id))

  if (any(can_install)) {
    pkgidx <- can_install[1]
    return(task("install", pkgidx = pkgidx))
  }

  ## Detect internal error
  if (!all(state$plan$install_done) && all(is.na(state$plan$worker_id))) {
    stop("Internal error, no task running and cannot select new task")
  }

  ## Looks like nothing else to do
  task("idle")
}

task <- function(name, ...) {
  list(name = name, args = list(...))
}

start_task <- function(state, task) {
  if (task$name == "idle") {
    state

  } else if (task$name == "package") {
    start_task_package(state, task)

  } else if (task$name == "build") {
    start_task_build(state, task)

  } else if (task$name == "install") {
    start_task_install(state, task)

  } else {
    stop("Unknown task, internal error")
  }
}

get_worker_id <- (function() {
  id <- 0
  function() {
    id <<- id + 1
    as.character(id)
  }
})()

make_build_process <- function(path, tmp_dir, lib, vignettes,
                               needscompilation, binary) {

  ## with_libpath() is needed for newer callr, which forces the current
  ## lib path in the child process.
  withr::with_libpaths(lib, action = "prefix",
    pkgbuild_process$new(
      path, tmp_dir, binary = binary, vignettes = vignettes,
      needs_compilation = needscompilation, compile_attributes = FALSE,
      args = if (binary) glue("--library={lib}"))
    )
}

start_task_package <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]

  state$plan$package_time[[pkgidx]] <- Sys.time()
  alert("info", "Packaging {pkg {pkg}} {version {version}}")

  if (file.info(path)$isdir) {
    ## Just build tree_dir
    task$args$tree_dir <- path
    start_task_package_build(state, task)
  } else {
    ## Uncompress to tree_dir, then build it
    task$args$tree_dir <- paste0(path, "-tree")
    start_task_package_uncompress(state, task)
  }
}

start_task_package_uncompress <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]

  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  lib <- state$config$lib

  task$args$phase <- "uncompress"
  px <- make_uncompress_process(path, task$args$tree_dir)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character(), stderr = character())
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state
}

start_task_package_build <- function(state, task) {
  pkgidx <- task$args$pkgidx

  ## The actual package might be in a subdirectory, e.g. when the
  ## tree was downloaded from GitHub
  tree_dir <- task$args$tree_dir
  dir_tree_dir <- dir(tree_dir)
  if (! "DESCRIPTION" %in% dir_tree_dir && length(dir_tree_dir) == 1 &&
      "DESCRIPTION" %in% dir(file.path(tree_dir, dir_tree_dir))) {
    tree_dir <- file.path(tree_dir, dir_tree_dir)
  }

  vignettes <- state$plan$vignettes[pkgidx]
  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  lib <- state$config$lib

  task$args$phase <- "build"
  px <- make_build_process(tree_dir, dirname(tree_dir), lib, vignettes,
                           needscompilation, binary = FALSE)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character(), stderr = character())
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$build_time[[pkgidx]] <- Sys.time()
  state
}

#' @importFrom pkgbuild pkgbuild_process

start_task_build <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]
  vignettes <- state$plan$vignettes[pkgidx]
  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  tmp_dir <- create_temp_dir()
  lib <- state$config$lib

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  alert("info", "Building {pkg {pkg}} {version {version}}")

  px <- make_build_process(path, tmp_dir, lib, vignettes, needscompilation,
                           binary = TRUE)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character(), stderr = character())
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$build_time[[pkgidx]] <- Sys.time()
  state
}

start_task_install <- function(state, task) {
  pkgidx <- task$args$pkgidx
  filename <- state$plan$file[pkgidx]
  lib <- state$config$lib
  metadata <- state$plan$metadata[[pkgidx]]

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  update_progress_bar(state)

  px <- make_install_process(filename, lib = lib, metadata = metadata)
  worker <- list(
    id = get_worker_id(), task = task, process = px,
    stdout = character(), stderr = character())

  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$install_time[[pkgidx]] <- Sys.time()
  state
}

stop_task <- function(state, worker) {
  if (worker$task$name == "package") {
    stop_task_package(state, worker)

  } else if (worker$task$name == "build") {
    stop_task_build(state, worker)

  } else if (worker$task$name == "install") {
    stop_task_install(state, worker)

  } else {
    stop("Unknown task, internal error")
  }
}

stop_task_package <- function(state, worker) {
  if (worker$task$args$phase == "uncompress") {
    stop_task_package_uncompress(state, worker)
  } else {
    stop_task_package_build(state, worker)
  }
}

stop_task_package_uncompress <- function(state, worker) {
  pkgidx <- worker$task$args$pkgidx
  success <- worker$process$get_exit_status() == 0

  if (!success) {
    pkg <- state$plan$package[pkgidx]
    version <- state$plan$version[pkgidx]
    time <- Sys.time() - state$plan$package_time[[pkgidx]]
    ptime <- pretty_sec(as.numeric(time, units = "secs"))
    alert("danger", "Failed to package {pkg {pkg}} \\
           {version {version}} {timestamp {ptime}}")
    update_progress_bar(state, 1L)

    state$plan$package_done[[pkgidx]] <- TRUE
    state$plan$package_time[[pkgidx]] <- time
    state$plan$package_error[[pkgidx]] <- ! success
    state$plan$package_stdout[[pkgidx]] <- worker$stdout
    state$plan$package_stderr[[pkgidx]] <- worker$stderr
    state$plan$worker_id[[pkgidx]] <- NA_character_

    throw(new_error(
      "Failed to uncompress {pkg} from {state$plan$file[[pkgidx]]}."))
  }

  start_task_package_build(state, worker$task)
}

stop_task_package_build <- function(state, worker) {
  pkgidx <- worker$task$args$pkgidx
  success <- worker$process$get_exit_status() == 0

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$package_time[[pkgidx]]
  ptime <- pretty_sec(as.numeric(time, units = "secs"))

  if (success) {
    alert("success", "Packaged {pkg {pkg}} {version {version}} \\
           {timestamp {ptime}}")
    ## Need to save the name of the built package
    state$plan$file[pkgidx] <- worker$process$get_built_file()
  } else {
    alert("danger", "Failed to package {pkg {pkg}} \\
           {version {version}} {timestamp {ptime}}")
  }
  update_progress_bar(state, 1L)

  state$plan$package_done[[pkgidx]] <- TRUE
  state$plan$package_time[[pkgidx]] <- time
  state$plan$package_error[[pkgidx]] <- ! success
  state$plan$package_stdout[[pkgidx]] <- worker$stdout
  state$plan$package_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    throw(new_error(c("Failed to package {pkg} from source tree ",
                      "{state$plan$file[[pkgidx]]}.")))
  }

  state
}

#' @importFrom prettyunits pretty_sec

stop_task_build <- function(state, worker) {

  ## TODO: make sure exit status is non-zero on build error!
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$build_time[[pkgidx]]
  ptime <- pretty_sec(as.numeric(time, units = "secs"))

  if (success) {
    alert("success", "Built {pkg {pkg}} {version {version}} \\
           {timestamp {ptime}}")
    ## Need to save the name of the built package
    state$plan$file[pkgidx] <- worker$process$get_built_file()
  } else {
    alert("danger", "Failed to build {pkg {pkg}} \\
           {version {version}} {timestamp {ptime}}")
  }
  update_progress_bar(state, 1L)

  state$plan$build_done[[pkgidx]] <- TRUE
  state$plan$build_time[[pkgidx]] <- time
  state$plan$build_error[[pkgidx]] <- ! success
  state$plan$build_stdout[[pkgidx]] <- worker$stdout
  state$plan$build_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    throw(new_error("Failed to build source package {pkg}."))
  }

  state
}

installed_note <- function(pkg) {

  standard_note <- function() {
    if (pkg$type %in% c("cran", "standard")) {
      ""
    } else {
      paste0("(", pkg$type, ")")
    }
  }

  github_note <- function() {
    meta <- pkg$metadata[[1]]
    paste0("(github::", meta[["RemoteUsername"]], "/", meta[["RemoteRepo"]],
           "@", substr(meta[["RemoteSha"]], 1, 7), ")")
  }

  switch(
    pkg$type,
    cran = "",
    bioc = "(BioC)",
    standard = standard_note(),
    local = "(local)",
    github = github_note()
  )
}

#' @importFrom prettyunits pretty_sec

stop_task_install <- function(state, worker) {

  ## TODO: make sure the install status is non-zero on exit
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$install_time[[pkgidx]]
  ptime <- pretty_sec(as.numeric(time, units = "secs"))
  note <- installed_note(state$plan[pkgidx,])

  if (success) {
    alert("success", "Installed {pkg {pkg}} \\
             {version {version}} {note} {timestamp {ptime}}")
  } else {
    alert("danger", "Failed to install  {pkg pkg}} {version {version}}")
  }
  update_progress_bar(state, 1L)

  state$plan$install_done[[pkgidx]] <- TRUE
  state$plan$install_time[[pkgidx]] <- time
  state$plan$install_error[[pkgidx]] <- ! success
  state$plan$install_stdout[[pkgidx]] <- worker$stdout
  state$plan$install_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    throw(new_error("Failed to install binary package {pkg}."))
  }

  ## Need to remove from the dependency list
  state$plan$deps_left <- lapply(state$plan$deps_left, setdiff, pkg)

  state
}

create_install_result <-  function(state) {
  result <- state$plan
  class(result) <- c("pkginstall_result", class(result))
  result
}

#' @export
#' @importFrom prettyunits pretty_sec

print.pkginstall_result <- function(x, ...) {
  newly <- sum(x$lib_status == "new")
  upd   <- sum(x$lib_status == "update")
  noupd <- sum(x$lib_status == "no-update")
  curr  <- sum(x$lib_status == "current")
  if (newly) cat("Installed: ",  newly, "\n", sep = "")
  if (upd)   cat("Updated: ",    upd,   "\n", sep = "")
  if (noupd) cat("Not updated:", noupd, "\n", sep = "")
  if (curr)  cat("Current: ",    curr,  "\n", sep = "")

  ## TODO
  build_time <- sum(unlist(x$build_time), na.rm = TRUE)
  inst_time <- sum(unlist(x$install_time), na.rm = TRUE)

  cat("Build time:  ", pretty_sec(build_time), "\n", sep = "")
  cat("Intall time: ", pretty_sec(inst_time), "\n", sep = "")

  invisible(x)
}

kill_all_processes <- function(state) {
  alive <- FALSE
  for (i in seq_along(state$workers)) {
    proc <- state$workers[[i]]$process
    proc$signal(tools::SIGINT)
    alive <- alive || proc$is_alive()
  }

  if (alive) {
    for (i in seq_along(state$workers)) {
      proc <- state$workers[[i]]$process
      proc$wait(200)
      proc$kill_tree()
    }
  }
}
