
sysreqs_resolve <- function(sysreqs, os = NULL, os_release = NULL,
                            config = NULL) {
  if (is.null(os) || is.null(os_release)) {
    lnx <- detect_linux()
    os <- os %||% lnx$distribution
    os_release <- os_release %||% lnx$release
  }
  config <- config %||% current_config()
  synchronise(sysreqs_async_resolve(sysreqs, os, os_release, config))
}

sysreqs_async_resolve <- function(sysreqs, os, os_release, config) {
  sysreqs; os; os_release; config
  sysreqs_async_resolve_query(sysreqs, os, os_release, config)$
    then(function(resp) {
      if (resp$status_code < 400) return(resp)
      throw(pkg_error(
        call. = FALSE,
        "Failed to look up system requirements for OS {os} {os_release}.",
        i = "HTTP error {resp$status_code} for {.url {resp$url}}.",
        i = "Response: {.val {rawToChar(resp$content)}}."
      ))
    })$
      then(function(resp) sysreqs_resolve_process(sysreqs, os, os_release, resp))$
      then(function(res) add_class(res, "pkg_sysreqs_result"))
}

sysreqs_async_resolve_query <- function(sysreqs, os, os_release, config) {
  config <- config %||% current_config()
  rspm <- config$get("sysreqs_rspm_url")
  rspm_repo_id <- config$get("sysreqs_rspm_repo_id")
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)

  req_url <- sprintf(
    "%s/sysreqs?distribution=%s&release=%s",
    rspm_repo_url,
    os,
    os_release
  )

  headers <- c("Content-Type" = "text/plain")

  data <- sysreqs_resolve_make_data(sysreqs)

  http_post(req_url, data = data, headers = headers)
}

sysreqs_resolve_process <- function(sysreqs, os, os_release, resp) {
  hdr <- curl::parse_headers_list(resp$headers)
  cnt <- rawToChar(resp$content)
  Encoding(cnt) <- "UTF-8"

  data <- jsonlite::fromJSON(cnt, simplifyVector = FALSE)

  pre_install <- unique(as.character(unlist(c(
    data[["pre_install"]],
    lapply(data[["dependencies"]], `[[`, "pre_install")
  ))))
  install_scripts <- unique(as.character(unlist(c(
    data[["install_scripts"]],
    lapply(data[["dependencies"]], `[[`, "install_scripts")
  ))))
  post_install <- unique(as.character(unlist(c(
    data[["post_install"]],
    lapply(data[["dependencies"]], `[[`, "post_install")
  ))))

  list(
    os = os,
    os_release = os_release,
    url = resp$url,
    total = resp$times["total"],
    pre_install = pre_install,
    install_scripts = install_scripts,
    post_install = post_install
  )
}

sysreqs_canonise_query <- function(sysreqs) {
  sysreqs <- str_trim(sysreqs)
  sysreqs <- sort(unique(sysreqs[!is.na(sysreqs) & sysreqs != ""]))
  sysreqs <- gsub("\n", "\n ", sysreqs)
  sysreqs
}

sysreqs_resolve_make_data <- function(sysreqs) {
  sysreqs <- sysreqs_canonise_query(sysreqs)
  paste(collapse = "\n", c(
    "Package: pkgdependssysreqs",
    "Version: 1.0.0",
    "SystemRequirements: ",
    paste0("    ", sysreqs),
    "Note: and thank you!",
    ""
  ))
}

sysreqs_install <- function(sysreqs_cmds, config = NULL) {
  config <- config %||% current_config()
  sudo <- config$get("sysreqs_sudo")
  verbose <- config$get("sysreqs_verbose")
  dry_run <- config$get("sysreqs_dry_run")

  cmds <- unlist(sysreqs_cmds[c(
    "pre_install",
    "install_scripts",
    "post_install"
  )])
  if (length(cmds) == 0) return()

  cli::cli_alert_info("Installing system requirements")

  # TODO: fix 'R' commands (e.g. `R CMD javareconf`) to call the current
  # version of R and not the one on the PATH

  if (dry_run) cmds <- paste("echo", cmds)

  if (verbose) {
    callback <- function(x, ...) {
      x <- str_trim(x)
      if (nchar(x)) cli::cli_verbatim(x)
    }
  } else {
    callback <- function(x, ...) invisible()
  }

  output <- lapply(cmds, function(cmd) {
    if (sudo) {
      sh <- "sudo"                                               # nocov
      cmdline <- c("sh", "-c", cmd)                              # nocov
    } else {
      sh <- "sh"
      cmdline <- c("-c", cmd)
    }
    fullcmd <- paste(c(sh, cmdline), collapse = " ")
    cli::cli_alert_info("Executing {.code {fullcmd}}")
    processx::run(
      sh,
      cmdline,
      stdout_callback = callback,
      stderr_to_stdout = TRUE
    )
  })

  invisible(output)
}

detect_linux <- function() {
  plt <- pkgcache::current_r_platform_data()
  list(
    distribution = plt[["distribution"]] %||% "unknown",
    release = plt[["release"]] %||% "unknown"
  )
}
