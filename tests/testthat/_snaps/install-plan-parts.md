# handle_event, process still running

    Code
      state <- start_task_build(state, task("build", pkgidx = 1))
    Message <cliMessage>
      i Building R6 2.2.2

# handle_event, build process finished

    Code
      state <- start_task_build(state, task("build", pkgidx = 1))
    Message <cliMessage>
      i Building R6 2.2.2

---

    Code
      repeat {
        events <- poll_workers(state)
        state <- handle_events(state, events)
        if (all(state$plan$build_done)) break
      }
    Message <cliMessage>
      v Built R6 2.2.2

# handle event, build process finished, but failed

    Code
      state <- make_start_state(plan, list(foo = "bar"))

---

    Code
      state <- start_task_install(state, task("install", pkgidx = 1))

---

    Code
      repeat {
        events <- poll_workers(state)
        state <- handle_events(state, events)
        if (all(state$plan$build_done)) break
      }
    Message <cliMessage>
      x Failed to install R6 2.2.2
    Error <package_install_error>
      ! Failed to install binary package 'R6'.

# handle_event, install process finished

    Code
      repeat {
        events <- poll_workers(state)
        state <- handle_events(state, events)
        if (done) break
        if (!proc$is_alive()) done <- TRUE
      }
    Message <cliMessage>
      v Installed R6 2.2.2 

# handle event, install process finished, but failed

    Code
      done <- FALSE
      repeat {
        events <- poll_workers(state)
        state <- handle_events(state, events)
        if (done) break
        if (!proc$is_alive()) done <- TRUE
      }
    Message <cliMessage>
      x Failed to install R6 2.2.2
    Error <package_install_error>
      ! Failed to install binary package 'R6'.

# deadlock detection

    Code
      install_package_plan(plan, lib = tempfile())
    Error <simpleError>
      Internal pkgdepends error, no task running and cannot select new task

# install_args are passed

    Code
      install_package_plan(plan, lib = lib)
    Message <cliMessage>
      i Building foo 
      v Built foo 
      v Installed foo  (local)
      v Summary:

# installed_note

    Code
      installed_note(list(type = "cran"))
    Output
      [1] ""
    Code
      installed_note(list(type = "bioc"))
    Output
      [1] "(Bioconductor)"
    Code
      installed_note(list(type = "standard"))
    Output
      [1] ""
    Code
      installed_note(list(type = "local"))
    Output
      [1] "(local)"
    Code
      installed_note(list(type = "github", metadata = list(list(RemoteUsername = "r-lib",
        RemoteRepo = "pak", RemoteSha = "5a4da54df42528545af8a64e83112be21273907c6dfa0f31a0982ca88db6527d"))))
    Output
      [1] "(github::r-lib/pak@5a4da54)"

