
#' @importFrom cli is_utf8_output

progress_chars <- function() {
  if (is_utf8_output()) {
    list(
      lpar = "\u2e28",
      rpar = "\u2e29",
      fill = "\u2588")

  } else {
    list(
      lpar = "(",
      rpar = ")",
      fill = "#")
  }
}

#' @importFrom cli get_spinner
#' @importFrom cliapp cli_progress_bar

res__create_progress_bar <- function(self, private) {
  bar <- list()
  bar$spinner <- get_spinner()
  bar$spinner_state <- 1L
  bar$chars <- progress_chars()

  bar$bar <- cli_progress_bar(
    format = ":xbar:xstate :xspinner :xmsg",
    total = 10e7,
    force = TRUE
    )

  bar
}

res__update_progress_bar <- function(self, private) {
  deps <- nrow(private$state)
  direct <- sum(private$state$direct)
  direct_done <- sum(!is.na(private$state$status) & private$state$direct)

  bar <- if (direct >= 5) {
    make_bar(private$bar$chars, direct_done / direct, width = 15)
  }

  tokens <- list(
    xbar = bar %||% "",
    xstate = make_progress_main(deps, direct_done, direct),
    xspinner = make_progress_spinner(self, private),
    xmsg = make_trailing_progress_msg(self, private)
  )

  private$bar$bar$tick(0, tokens = tokens)
}

make_bar <- function(chars, p, width =  15) {
  width <- width - 2L

  w <- if (isTRUE(all.equal(p, 1))) width else trunc(width * p)

  pchars <- rep(chars$fill, w)
  xchars <- rep("\u00a0", max(width - w, 0))
  bar <- paste(
    c(chars$lpar, pchars, xchars, chars$rpar, "\u00a0"),
    collapse = "")

  ## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  ## It seems that this bug has crept back, so we don't color in RStudio
  if (! is_rstudio()) {
    crayon::green(bar)
  } else {
    crayon::green(bar)
  }
}

make_progress_main <- function(deps, done, total) {
  bggrey <- crayon::make_style("grey", bg = TRUE)
  paste0(
    "Found ",
    bggrey(crayon::black(paste0(" ", deps, " "))),
    " deps for ",
    bggrey(crayon::black(paste0(" ", done, "/", total, " "))),
    " pkgs"
  )
}

make_progress_spinner  <- function(self, private) {
  bar <- private$bar
  spin <- bar$spinner$frames[[bar$spinner_state]]
  bar$spinner_state <-
    bar$spinner_state %% length(bar$spinner$frames) + 1L
  private$bar <- bar
  paste0("[", spin, "]")
}

make_trailing_progress_msg <- function(self, private) {
  ongoing <- private$state[is.na(private$state$status), ]
  if (nrow(ongoing) == 0) return("Done")

  types <- vcapply(ongoing$remote, "[[", "type")
  remote <- if (all(types %in% c("cran", "bioc", "standard"))) {
    ongoing$remote[[ order(ongoing$started_at)[1] ]]
  } else {
    nonstd <- ongoing[! types %in% c("cran", "bioc", "special"), ]
    nonstd$remote[[ order(nonstd$started_at)[1] ]]
  }

  if (remote$type %in% c("cran", "bioc", "standard")) {
    "Resolving standard (CRAN/BioC) packages"
  } else if (remote$type %in% "installed") {
    "Checking installed packages"
  } else {
    paste0("Resolving ", remote$ref)
  }
}

res__done_progress_bar <- function(self, private) {
  private$bar$bar$terminate()
}
