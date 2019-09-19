
repoman_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

`%|z|%` <- function(l, r) {
  if (identical(l, "")) r else l
}

get_platform <- function() {
  .Platform
}

current_r_platform <- function() {
  type <- get_platform()$pkgType
  if (!is_string(type))
    "source"
  else if (grepl("^mac", type)) {
    "macos"
  } else if (grepl("^win", type)) {
    "windows"
  } else {
    "source"
  }
}

default_platforms <- function() unique(c(current_r_platform(), "source"))

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

  } else if (status == "Had") {
    obj$bytes <- as.double(bytes)
  }

  obj
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

get_all_package_dirs <- function(platforms, rversions) {
  minors <- unique(get_minor_r_version(rversions))
  res <- lapply(platforms, function(pl) {
    if (pl == "source") {
      cbind("source", "*", "src/contrib")

    } else if (pl == "windows") {
      cbind("windows", minors, paste0("bin/windows/contrib/", minors))

    } else if (pl == "macos") {
      res1 <- lapply(minors, function(v) {
        if (package_version(v) <= "2.15") {
          cbind("macos", v, paste0("bin/macosx/leopard/contrib/", v))
        } else if (package_version(v) == "3.0") {
          cbind("macos", v, paste0("bin/macosx/contrib/", v))
        } else if (package_version(v) <= "3.2") {
          cbind("macos", v, paste0(c("bin/macosx/contrib/",
                                     "bin/macosx/mavericks/contrib/"), v))
        } else if (package_version(v) == "3.3") {
          cbind("macos", v, paste0("bin/macosx/mavericks/contrib/", v))
        } else {
          cbind("macos", v, paste0("bin/macosx/el-capitan/contrib/", v))
        }
      })
      do.call(rbind, res1)
    }
  })

  mat <- do.call(rbind, res)
  colnames(mat) <- c("platform", "rversion", "contriburl")
  res <- as_tibble(mat)
  res$prefix <- paste0(
    "/",
    ifelse(res$rversion == "*", "*", paste0("R-", res$rversion)),
    "/", res$platform, "/"
  )

  res
}

same_sha <- function(s1, s2) {
  assert_that(is_string(s1), is_string(s2))
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

is_verbose <- function() {
  env <- Sys.getenv("R_PKG_SHOW_PROGRESS", "")
  if (env != "") {
    tolower(env) == "true"
  } else {
    opt <- getOption("pkg.show_progress")
    if (!is.null(opt)) {
      return(isTRUE(opt))
    } else {
      interactive()
    }
  }
}

add_attr <- function(x, attr, value) {
  attr(x, attr) <- value
  x
}

detect_download_cache_dir <- function() {
  tempfile()
}

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- as_tibble(structure(
        replicate(
          length(miss_cols),
          if (nrow(data[[i]])) NA else logical(),
          simplify = FALSE),
        names = miss_cols))
      data[[i]] <- as_tibble(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}

drop_nulls <- function(x) {
  x[! vlapply(x, is.null)]
}

## R CMD check fixes
self <- private <- "foobar"

is_rstudio_version <- function(ver) {
  tryCatch(
    rstudioapi::getVersion() >=ver,
    error = function(e) FALSE
  )
}

have_rstudio_bug_2387 <- function() {
  if (!is.null(r <- repoman_data$rstudio_bug_2387)) return(r)
  r <- repoman_data$rstudio_bug_2387 <-
    Sys.getenv("RSTUDIO", "") != "" &&
    Sys.getenv("RSTUDIO_TERM", "") == "" &&
    !is_rstudio_version("1.2.128")
  r
}

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
