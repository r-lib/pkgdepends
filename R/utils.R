
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

  tibble(
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

is_na_scalar <- function(x) {
  length(x) == 1 && is.na(x)
}

#' @importFrom cli symbol

progress_msg <- function(msg, status = c("tick", "cross")) {
  status <- match.arg(status)
  cat(symbol[[status]], " ", msg, "\n", sep = "")
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

  res <- as_tibble(do.call(rbind, res), validate = FALSE)
  colnames(res) <- c("platform", "rversion", "contriburl")
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
