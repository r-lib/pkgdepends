
remotes__create_progress_bar <- function(self, private, what) {
  if (!is_verbose()) return(NULL)
  bar <- list()

  what$status <- NA_character_
  what$size <- NA_integer_
  what$current <- NA_integer_
  what$finished_at <- NA_real_

  bar$what <- what[, c("ref", "status", "size", "current", "finished_at")]
  bar$spinner <- get_spinner()
  bar$spinner_state <- 1L
  bar$chars <- progress_chars()

  app <- default_app() %||% start_app()
  bar$bar <- app$progress_bar(
    show_after = 0,
    format = ":xbar :xpkgs | :xbytes | :xspin :xmsg",
    total = nrow(what),
    force = TRUE)
  bar
}

remotes__update_progress_bar <- function(self, private, idx, data) {
  if (!is_verbose()) return(NULL)

  if (identical(data, "done")) {
    private$progress_bar$what$status[idx] <- "DONE"
    private$progress_bar$what$finished_at[idx] <- Sys.time()
  } else {
    if (data$total) private$progress_bar$what$size[idx] <- data$total
    private$progress_bar$what$size[idx] <- data$current
    if (data$total && data$total == data$current) {
      private$progress_bar$what$status[idx] <- "DONE"
      private$progress_bar$what$finished_at[idx] <- Sys.time()
    }
  }

  bar <- private$progress_bar
  what <- bar$what
  pkg_done <- sum(!is.na(what$status))
  pkg_total <- nrow(what)
  percent <- pkg_done / pkg_total
  bytes_done <- sum(what$current, na.rm = TRUE)
  bytes_total <- sum(what$size, na.rm = TRUE)
  unknown <- sum(is.na(what$size) & is.na(what$status))

  tokens <- list(
    xbar = make_bar(bar$chars, percent, width = 15),
    xpkgs = make_progress_packages(pkg_done, pkg_total),
    xbytes = make_progress_bytes(bytes_done, bytes_total, unknown),
    xspin = make_spinner(private),
    xmsg = make_trailing_download_msg(what)
  )

  bar$bar$tick(0, tokens = tokens)
}

remotes__done_progress_bar <- function(self, private) {
  if (!is_verbose()) return()
  private$progress_bar$bar$terminate()
}

make_progress_packages <- function(done, total) {
  paste0(
    crayon::bgBlue(crayon::black(paste0(" ", done, "/", total, " "))),
    " pkgs"
  )
}

make_progress_bytes <- function(done, total, unknown) {
  if (total == 0) return(paste0(unknown, " pkgs with unknown size"))
  paste0(
    pretty_bytes(done), " / ", pretty_bytes(total),
    if (unknown) paste0(" + ", unknown, " unknown")
  )
}

make_spinner <- function(private)  {
  progress_bar <- private$progress_bar
  spin <- progress_bar$spinner$frames[[progress_bar$spinner_state]]
  progress_bar$spinner_state <-
    progress_bar$spinner_state %% length(progress_bar$spinner$frames) + 1L
  private$progress_bar <- progress_bar
  paste0("[", spin, "]")
}

make_trailing_download_msg <- function(tab) {
  if (all(is.na(tab$finished_at))) return("Working...")
  last <- which.max(tab$finished_at)
  paste0("Got ", tab$ref[last])
}
