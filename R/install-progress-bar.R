
pkg_data <- new.env()

#' @importFrom cli symbol
#' @importFrom cliapp cli_alert_success cli_alert_info cli_alert_warning
#'   cli_alert_danger cli_text

alert <- function(type, msg, .envir = parent.frame()) {
  if (!is_verbose()) return()
  if (have_rstudio_bug_2387()) {
    switch(
      type,
      success = cli_text(paste(symbol$tick, msg), .envir = .envir),
      info = cli_text(paste(symbol$info, msg), .envir = .envir),
      warning = cli_alert_warning(msg, .envir = .envir),
      danger = cli_alert_danger(msg, .envir = .envir)
    )
  } else {
    switch (
      type,
      success = cliapp::cli_alert_success(msg, .envir = .envir),
      info = cli_alert_info(msg, .envir = .envir),
      warning = cli_alert_warning(msg, .envir = .envir),
      danger = cli_alert_danger(msg, .envir = .envir)
    )
  }
}

#' @importFrom cli get_spinner

create_progress_bar <- function(state) {
  if (!is_verbose()) return()
  pkg_data$spinner <- get_spinner()
  pkg_data$spinner_state <- 1L

  cli_progress_bar(
    format = ":xbar ETA :eta | :xbuilt | :xinst | :xmsg",
    total = sum(!state$plan$build_done) + sum(!state$plan$install_done),
    force = TRUE
  )
}

update_progress_bar <- function(state, tick = 0) {

  if (!is_verbose()) return()

  plan <- state$plan
  total <- nrow(plan)
  installed <- sum(plan$install_done)
  built <- sum(plan$build_done)

  building <- sum(buildingl <- !plan$build_done & !is.na(plan$worker_id))
  installing <- sum(!buildingl & !is.na(plan$worker_id))

  ## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  pp <- if (Sys.getenv("RSTUDIO", "") == "" ||
            Sys.getenv("RSTUDIO_TERM", "") != "") {
    function(x) x
  } else {
    function(x) crayon::strip_style(x)
  }

  chars <- progress_chars()
  tokens <- list(
    xbar = pp(make_install_bar(installed / total, built/total, width =  15)),
    xbuilt = pp(make_progress_block(chars$build, built, total, building)),
    xinst = pp(make_progress_block(chars$inst, installed, total, installing)),
    xmsg = pp(make_install_trailing_progress_msg(state))
  )

  saveRDS(tokens, "/tmp/tok.rds")

  state$progress$tick(tick, tokens = tokens)
}

## p1 <= p2 must hold

make_install_bar <- function(p1, p2, width) {
  width <- width - 2L

  w1 <- if (isTRUE(all.equal(p1, 1))) width else trunc(width * p1)
  w2 <- if (isTRUE(all.equal(p2, 1))) width - w1 else trunc(width * (p2 - p1))

  chars <- progress_chars()
  p1chars <- rep(chars$fill, w1)
  p2chars <- rep(chars$half, w2)
  xchars <- rep(" ", max(width - w1 - w2, 0))
  bar <- paste(
    c(chars$lpar, p1chars, p2chars, xchars, chars$rpar), collapse = "")

  ## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  if (Sys.getenv("RSTUDIO", "") == "" ||
      Sys.getenv("RSTUDIO_TERM", "") != "") {
    crayon::green(bar)
  } else {
    bar
  }
}

make_progress_block <- function(sym, done, total, prog) {
  spin <- pkg_data$spinner$frames[[pkg_data$spinner_state]]
  pkg_data$spinner_state <-
    pkg_data$spinner_state %% length(pkg_data$spinner$frames) + 1L
  paste0(
    sym, "  ",
    done, "/", total,
    if (prog) paste0(" ", spin, " ", prog) else "    "
  )
}

done_progress_bar <- function(state) {
  if (!is_verbose()) return()
  state$progress$terminate()
}

make_install_trailing_progress_msg <- function(state) {
  working <- !is.na(state$plan$worker_id)
  installing <- state$plan$build_done & working
  building <- !state$plan$build_done & working

  building_pkgs <- paste(state$plan$package[building], collapse = ", ")
  installing_pkgs <- paste(state$plan$package[installing], collapse = ", ")

  paste0(
    if (any(building)) paste0("building ", building_pkgs),
    if (any(building) && any(installing)) ", ",
    if (any(installing)) paste0("installing ", installing_pkgs)
  )
}
