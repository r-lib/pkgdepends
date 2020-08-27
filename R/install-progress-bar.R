
#' @importFrom cli symbol cli_alert_success cli_alert_danger

alert <- function(type, msg, .envir = parent.frame()) {
  switch (
    type,
    success = cli_alert_success(msg, .envir = .envir),
    info = cli_alert_info(msg, .envir = .envir),
    warning = cli_alert_warning(msg, .envir = .envir),
    danger = cli_alert_danger(msg, .envir = .envir)
  )
}

#' @importFrom cli get_spinner cli_status

create_progress_bar <- function(state) {
  bar <- new.env(parent = emptyenv())
  bar$spinner <- get_spinner()
  bar$spinner_state <- 1L
  bar$status <- cli_status("Installing packages...", .auto_close = FALSE)
  bar
}

#' @importFrom cli cli_status_update

update_progress_bar <- function(state, tick = 0) {
  if (!isTRUE(getOption("pkg.show_progress", FALSE))) {
    return()
  }

  plan <- state$plan
  total <- nrow(plan)
  installed <- sum(plan$install_done)
  built <- sum(plan$build_done)

  building <- sum(buildingl <- !plan$build_done & !is.na(plan$worker_id))
  installing <- sum(!buildingl & !is.na(plan$worker_id))

  chars <- progress_chars()

## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  ## https://github.com/rstudio/rstudio/issues/7278
  pp <- if (rstudio$detect()$type == "rstudio_console") {
    function(x) gsub(" ", chars$space, crayon::strip_style(x), fixed = TRUE)
  } else {
    function(x) gsub(" ", chars$space, x, fixed = TRUE)
  }

  xbar <- pp(make_install_bar(installed / total, built/total, width =  15))
  xbuilt  <- pp(make_progress_block(state, chars$build, built, total, building))
  xinst <- pp(make_progress_block(state, chars$inst, installed, total, installing))
  xmsg <- pp(make_install_trailing_progress_msg(state))

  cli_status_update(
    state$bar$status,
    gsub(" ", chars$space, "{xbar} | {xbuilt} | {xinst} | {xmsg}")
  )
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

make_progress_block <- function(state, sym, done, total, prog) {
  prgs <- state$progress
  spin <- prgs$spinner$frames[[prgs$spinner_state]]
  prgs$spinner_state <- prgs$spinner_state %% length(prgs$spinner$frames) + 1L
  paste0(
    sym, "  ",
    done, "/", total,
    if (prog) paste0(" ", spin, " ", prog) else "    "
  )
}

#' @importFrom cli cli_status_clear

done_progress_bar <- function(state) {
  cli_status_clear(state$progress$status)
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
