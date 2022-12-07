
roxy_to_rd <- function(text) {
  roxy_text <- paste0(
    "#' @name foo\n",
    "#' @title title\n",
    "#' @details\n",
    "#' ", gsub("\n", "\n#' ", text), "\n",
    "#' @md\n",
    "NULL\n"
  )
  # We change the current package's name to make sure that we create
  # cross-references that are fully qualified.
  pkg <- asNamespace("roxygen2")$roxy_meta_get("current_package")
  asNamespace("roxygen2")$roxy_meta_set("current_package", "no-package")
  on.exit(
    asNamespace("roxygen2")$roxy_meta_set("current_package", pkg),
    add = TRUE
  )
  # Suppress the warning caused by the changed package name
  out <- suppressWarnings(asNamespace("roxygen2")$roc_proc_text(
    asNamespace("roxygen2")$rd_roclet(),
    roxy_text
  ))

  det <- out$foo$get_rd("details")
  sec <- out$foo$get_rd("section")
  if (det == "NULL" && sec == "NULL") {
    cli::cli_alert_warning("No content in {.fn roxy_to_rd}.")
    ""
  } else if (det == "NULL") {
    sec
  } else if (sec == "NULL") {
    if (startsWith(det, "\\details")) {
      det <- sub("^[\\]details[{]\n?", "", det)
      det <- sub("\n?[}]$", "", det)
    }
    det
  } else {
    cli::cli_alert_warning("\\details{{}} content ignored in {.fn roxy_to_rd}.")
    sec
  }
}

generate_config_docs <- function() {

  # for the dynamic help in pak
  nms <- names(pkgdepends_config)
  dcs <- lapply(pkgdepends_config, function(x) x[["docs_pak"]] %||% x[["docs"]])
  inc <- vlapply(pkgdepends_config, function(x) x$pak %||% TRUE)
  nms <- nms[inc]
  dcs <- dcs[inc]

  rd <- lapply(dcs, roxy_to_rd)
  outfile <- file.path("inst/docs/pak-config-docs.rds")
  cli::cli_alert_info("Writing {.path {outfile}}")
  saveRDS(rd, outfile, version = 2)

  # for roxygen2 in pkgdepends
  items <- map_named(pkgdepends_config, function(name, entry) {
    paste0("* `", name, "`: ", entry$docs)
  })

  alldocs <- paste(items, collapse = "\n")
  alldocs
}

doc_share_rmd <- function(rmd, rds) {
  withr::local_envvar(THIS_IS_PAK = "true")
  cmd <- paste0("```{r child=\"", rmd, "\"}\n```\n")
  rd <- roxy_to_rd(cmd)
  saveRDS(rd, rds, version = 2)
  return("")
}
