
dependency_fields <- c("Depends", "Imports", "Suggests", "LinkingTo",
                       "Enhances")

repoman_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

first_existing_file <- function(...) {
  files <- unlist(list(...))
  ex <- file.exists(files)
  if (any(ex)) files[which(ex)[1]] else NULL
}

`%|NA|%` <- function(l, r) {
  if (identical(l, NA) || identical(l, NA_character_) ||
      identical(l, NA_integer_) || identical(l, NA_real_) ||
      identical(l, NA_complex_)) {
    r
  } else {
    l
  }
}

current_r_platform <- function() {
  if (grepl("^mac", .Platform$pkgType)) {
    "macos"
  } else if (grepl("^win", .Platform$pkgType)) {
    "windows"
  } else {
    "source"
  }
}

default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (!is.na(mirror)) mirror else "https://cran.rstudio.com"
}

current_r_version <- function() {
  as.character(getRversion())
}

get_minor_r_version <- function(x) {
  x <- package_version(x)
  vapply(unclass(x), function(x) paste(x[1:2], collapse = "."), character(1))
}

read.dcf.gz <- function(x) {
  con <- gzfile(x, open = "r")
  on.exit(close(con))
  read.dcf(con)
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

#' @importFrom utils installed.packages

base_packages <- function() {
  if (is.null(repoman_data$base_packages)) {
    repoman_data$base_packages <-
      rownames(installed.packages(priority = "base"))
  }
  repoman_data$base_packages
}

lapply_with_names <- function(X, FUN, ...) {
  structure(
    lapply(X, FUN, ...),
    names = names(X) %||% (if (is.character(X)) X)
  )
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = integer(1), ...)
}

vdapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = double(1), ...)
}

update_named_vector <- function(old, new) {
  comm <- intersect(names(old), names(new))
  add <- setdiff(names(new), names(old))
  old[comm] <- new[comm]
  old <- c(old, new[add])
  old
}

make_dl_status <- function(status, resolution, url, target, bytes,
                           error = NULL) {
  obj <- list(
    status = status,
    url = url,
    target = target,
    bytes = NA_real_,
    error = NULL
  )

  if (status == "Got") {
    obj$bytes <- as.double(bytes)

  } else if (status == "Failed") {
    obj$error <- error

  } else if (status == "Had") {
    obj$bytes <- as.double(bytes)
  }

  obj
}

mkdirp <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

write_bin_atomic <- function(object, file) {
  tmp <- paste0(file, ".tmp")
  on.exit(try(unlink(tmp), silent = TRUE))
  writeBin(object, tmp)
  file.rename(tmp, file)
}

save_rds_atomic <- function(object, file, ...) {
  tmp <- paste(file, ".tmp")
  on.exit(try(unlink(tmp), silent = TRUE))
  saveRDS(object, tmp, ...)
  file.rename(tmp, file)
}

format_archive_rds <- function(ards) {

  files <- sub("^[^/]+/", "", unlist(lapply(ards, rownames)))

  data.frame(
    stringsAsFactors = FALSE,
    package = rep(names(ards), viapply(ards, nrow)),
    file = files,
    version = sub("^[^_]+_([-\\.0-9]+)\\.tar\\.gz$", "\\1", files),
    size = unlist(unname(lapply(ards, "[[", "size")))
  )
}

comma_wrap <- function(x, indent = 2, exdent = indent, sep = ", ") {
  w <- strwrap(paste(x, collapse = sep), indent = indent, exdent = exdent)
  paste(w, collapse = "\n")
}

make_error <- function(message, class = character(), call = NULL, ...) {
  structure(
    c(list(message = message, call = call), list(...)),
    class = c(class, "error", "condition")
  )
}

add_class <- function(x, cl) {
  class(x) <- c(cl, class(x))
  x
}

clean_package_deps <- function(deps, dependencies, last = FALSE) {
  pkgs <- deps[deps$type %in% dependencies, ]$package
  pkgs <- setdiff(pkgs, c("R", base_packages()))
  if (last && length(pkgs)) paste0(pkgs, "@last") else pkgs
}
