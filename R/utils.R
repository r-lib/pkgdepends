
pkgd_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

`%&&%` <- function(l, r) if (!is.null(l)) r

`%|z|%` <- function(l, r) if (is.null(l) || identical(l, "")) r else l

`%&z&%` <- function(l, r) if (length(l) > 0 && l != "") r else ""

get_private <- function(x) x$.__enclos_env__$private

default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
    "https://cran.rstudio.com"
  } else {
    mirror
  }
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

## CRAN does not want me to call installed.packages at all, so let's
## hardcode this for now.

base_packages <- function() {
  if (is.null(pkgd_data$base_packages)) {
    pkgd_data$base_packages <- c(
      "base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tcltk",
      "tools", "utils"
    )
  }
  pkgd_data$base_packages
}

recommended_packages <- function() {
  if (is.null(pkgd_data$recommended_packages)) {
    pkgd_data$recommended_packages <- c(
      "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
      "lattice", "MASS", "Matrix", "mgcv", "nlme", "nnet", "rpart",
      "spatial", "survival"
    )
  }
  pkgd_data$recommended_packages
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
  assert_that(all_named(old), all_named(new))
  comm <- intersect(names(old), names(new))
  add <- setdiff(names(new), names(old))
  old[comm] <- new[comm]
  old <- c(old, new[add])
  old
}

make_dl_status <- function(status, url, target, bytes, error = NULL) {
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

  } else if (grepl("^Had", status)) {
    obj$bytes <- as.double(bytes)
  }

  obj
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

is_na_scalar <- function(x) {
  length(x) == 1 && is.na(x)
}

omit_cols <- function(df, omit) {
  if (!length(omit)) {
    df
  } else {
    df[ , setdiff(names(df), omit), drop = FALSE]
  }
}

same_sha <- function(s1, s2) {
  assert_that(
    is.character(s1), length(s1) == 1,
    is.character(s2), length(s2) == 1
  )
  if (is.na(s1) || is.na(s2)) return(FALSE)
  assert_that(
    is_string(s1),
    is_string(s2)
  )

  len <- min(nchar(s1), nchar(s2))
  substr(s1, 1, len) == substr(s2, 1, len)
}

format_iso_8601 <- function (date) {
  format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

read_lines <- function(con, ...) {
  if (is.character(con)) {
    con <- file(con)
    on.exit(close(con))
  }
  readLines(con, ...)
}

all_ok <- function(x) {
  if (all(vcapply(x, "[[", "status") == "OK")) "OK" else "FAILED"
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

file.size <- function(x) {
  file.info(x)$size
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

zip_lists <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

zip_vecs <- function(...) {
  mapply(c, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

lapply_rows <-  function(df, fun, ...) {
  lapply(seq_len(nrow(df)), function(i) fun(df[i,], ...))
}

`%||%` <- function(l, r) if (is.null(l)) r else l

add_attr <- function(x, attr, value) {
  attr(x, attr) <- value
  x
}

detect_download_cache_dir <- local({
  dir <- NULL
  function() {
    if (is.null(dir)) dir <<- tempfile()
    dir
  }
})

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- as_data_frame(structure(
        replicate(
          length(miss_cols),
          if (nrow(data[[i]])) NA else logical(),
          simplify = FALSE),
        names = miss_cols))
      data[[i]] <- as_data_frame(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}

drop_nulls <- function(x) {
  x[! vlapply(x, is.null)]
}

## R CMD check fixes
self <- private <- "foobar"

get_num_workers <- function() {
  n <- tryCatch(
    suppressWarnings(as.integer(getOption("Ncpus", NA_integer_))),
    error = function(e) NA_integer_)

  if (length(n) != 1 || is.na(n)) {
    n <- tryCatch(
      ps::ps_cpu_count(logical = TRUE),
      error = function(e) NA_integer_)
  }

  if (is.na(n)) n <- 1L

  n
}

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

is_online <- local({
  online <- TRUE
  expires <- Sys.time()
  function() {
    if (is_rcmd_check()) return(FALSE)
    t <- Sys.time()
    if (t >= expires) {
      online <<- pingr::is_online()
      expires <<- t + as.difftime(10, units = "secs")
    }
    online
  }
})

hash <- function(obj) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  serialize(obj, con <- file(tmp, open = "wb"))
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  close(con)
  tools::md5sum(tmp)[[1]]
}

once_per_session <- local({
  seen <- character()
  function(expr) {
    h <- hash(substitute(expr))
    if (! h %in% seen) {
      seen <<- c(seen, h)
      expr
    }
  }
})

synchronise <- synchronize <- sy <- function(...) {
  asNamespace("pkgcache")$synchronise(...)
}

async_constant <- function(...) {
  asNamespace("pkgcache")$async_constant(...)
}

async_map <- function(...) {
  asNamespace("pkgcache")$async_map(...)
}

http_get <- function(...) {
  asNamespace("pkgcache")$http_get(...)
}

http_post <- function(...) {
  asNamespace("pkgcache")$http_post(...)
}

when_all <- function(...) {
  asNamespace("pkgcache")$when_all(...)
}

async <- function(...) {
  asNamespace("pkgcache")$async(...)
}

download_file <- function(...) {
  asNamespace("pkgcache")$download_file(...)
}

download_one_of <- function(...) {
  asNamespace("pkgcache")$download_one_of(...)
}

http_stop_for_status <- function(...) {
  asNamespace("pkgcache")$http_stop_for_status(...)
}

new_async_timer <- function(...) {
  asNamespace("pkgcache")$async_timer$new(...)
}

external_process <- function(...) {
  asNamespace("pkgcache")$external_process(...)
}

format_error_with_stdout <- function(x, ...) {
  msg <- conditionMessage(x)
  if (is.null(x$data$stdout)) {
    paste0(msg, " output not available")
  } else {
    out <- last_stdout_lines(x$data$stdout, "stdout + stderr", "OE> ")
    c(paste0(msg, out[1]), out[-1])
  }
}

last_stdout_lines <- function(lines, std, prefix = "E> ") {
  if (is_interactive()) {
    pref <- paste0(
      ", ", std, if (length(lines) > 10) " (last 10 lines)", ":")
    out <- paste0(prefix, utils::tail(lines, 10))
    c(pref, "", out)
  } else {
    out <- paste0(prefix, lines)
    c(paste0(", ", std, ":"), "", out)
  }
}

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

try_silently <- function(expr) {
  try(expr, silent = TRUE)
}

rimraf <- function(...) {
  x <- file.path(...)
  if ("~" %in% x) stop("Cowardly refusing to delete `~`")
  unlink(x, recursive = TRUE, force = TRUE)
}

strrep <- function(x, times) {
  x <- as.character(x)
  if (length(x) == 0L) return(x)
  r <- .mapply(
    function(x, times) {
      if (is.na(x) || is.na(times)) return(NA_character_)
      if (times <= 0L) return("")
      paste0(replicate(times, x), collapse = "")
    },
    list(x = x, times = times),
    MoreArgs = list()
  )

  res <- unlist(r, use.names = FALSE)
  Encoding(res) <- Encoding(x)
  res
}

is_windows <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "windows")
}

# This is a workaround for some RStudio bugs:
# https://github.com/r-lib/pkginstall/issues/42
# https://github.com/rstudio/rstudio/issues/2387
# https://github.com/rstudio/rstudio/issues/7278

is_older_rstudio <- function() {
  rs <- rstudio$detect()
  rs$type == "rstudio_console" &&
    !is.null(rs$version) &&
    rs$version <= "1.4.800"
}

col_align <- function(text, align = c("left", "center", "right")) {
  if (length(text) == 0) return(text)
  width <- max(crayon::col_nchar(text, type = "width"))
  crayon::col_align(text, align = align, width = width)
}

get_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

# tools::md5sum has issues with UTF=8 file names on Windows, <= R 4.0

safe_md5sum <- function(path) {
  stopifnot(length(path) == 1)
  tryCatch(
    tools::md5sum(path),
    error = function(err) {
      tmp <- tempfile()
      on.exit(unlink(tmp, force = TRUE, recursive = TRUE), add = TRUE)
      file.copy(path, tmp)
      structure(tools::md5sum(tmp), names = path)
    }
  )
}

# TODO: rewrite this in C in ps

get_euid <- function() {
  euid <- tryCatch(
    as.integer(processx::run("id", "-u")$stdout),
    error = function(e) NA_integer_
  )
  if (length(euid) != 1 || is.na(euid)) euid <- NA_integer_
  euid
}
