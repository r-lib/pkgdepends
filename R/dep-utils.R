
#' @export
#' @rdname pkg_dep_types

pkg_dep_types_hard <- function() c("Depends", "Imports", "LinkingTo")

#' @export
#' @rdname pkg_dep_types

pkg_dep_types_soft <- function() c("Suggests", "Enhances")

#' Possible package dependency types
#'
#' Hard dependencies are needed for a package to load, soft dependencies
#' are optional.
#'
#' @return A string vector of dependency types, capitalized.
#'
#' @family package dependency utilities
#' @export

pkg_dep_types <- function() c(pkg_dep_types_hard(), pkg_dep_types_soft())

fast_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), pkg_dep_types())
  ## as.character is for empty data frame, e.g. from empty BioC repos
  deps <- as.character(unlist(pkgs[, cols], use.names = FALSE))
  nna <- which(!is.na(deps))
  if (length(nna)) {
    not_na_deps <- deps[nna]
    sp <- strsplit(not_na_deps, ",", fixed = TRUE)
    ll <- sapply(sp, length, USE.NAMES = FALSE)
    sp <- unlist(sp, use.names = FALSE)
    parsed <- re_match(sp,
      paste0("^\\s*(?<package>[^(\\s]+)\\s*",
             "(?:\\((?<op>[^0-9\\s]+)\\s*(?<version>[^)\\s]+)\\))?\\s*$"))
    parsed$idx <- rep(rep(seq_len(no_pkgs), length(cols))[nna], ll)
    parsed$type <- rep(rep(cols, each = no_pkgs)[nna], ll)
    parsed$ref <- parsed$package
    parsed$upstream <- pkgs$Package[parsed$idx]
    parsed <- parsed[, c("upstream", "idx", "ref", "type", "package",
                         "op", "version")]
    parsed <- parsed[! parsed$package %in% base_packages(), ]
    parsed <- parsed[order(parsed$idx), ]

  } else {
    parsed <- data_frame(
      upstream = character(),
      idx = integer(),
      ref = character(),
      type = character(),
      package = character(),
      version = character(),
      op = character()
    )
  }

  parsed
}

fast_select_deps <- function(deps, which, dependencies) {
  res <- deps[deps$idx == which, ]
  res <- res[res$type %in% dependencies,
             c("ref", "type", "package", "op", "version")]
  res[! res$package %in% base_packages(), ]
}

make_null_deps <- function() {
  data_frame(ref = character(), type = character(), package = character(),
             op = character(), version = character())
}

parse_deps <- function(deps, type) {
  assert_that(length(deps) == length(type))
  deps <- lapply(strsplit(deps, ","), str_trim)
  rx <- paste0(
    "(?<type>)",
    "^\\s*",
    "(?<package>[^\\s]+)",
    "\\s*",
    "(?:[(](?<op>>|>=|==|<|<=)\\s*(?<version>[-0-9\\.]+)[)])?\\s*$"
  )
  base <- base_packages()
  lapply(seq_along(deps), function(i) {
    x <- omit_cols(re_match(deps[[i]], pattern = rx), c(".text", ".match"))
    x$type <- if (length(x$type) > 0) type[[i]] else character()
    x[! x$package %in% base, ]
  })
}

deps_from_desc <- function(deps, last) {
  op_ver <- strsplit(deps$version, "\\s+")
  deps$op <- vcapply(op_ver, "[", 1)
  deps$op[deps$op == "*"] <- ""
  deps$version <- vcapply(op_ver, "[", 2)
  deps$version[is.na(deps$version)] <- ""
  deps$ref <- paste0(deps$package, if (last) "@last")
  base <- base_packages()
  res <- as_data_frame(deps[
    !deps$package %in% base,
    c("ref", "type", "package", "op", "version")
  ])
  rownames(res) <- NULL
  res
}

parse_all_deps <- function(deps) {
  deps <- na.omit(deps)
  res <- do.call(rbind, parse_deps(deps, names(deps)))
  if (is.null(res)) res <- parse_deps("", "")[[1]]
  res$ref <- res$package
  res[, c("ref", setdiff(names(res), "ref"))]
}

resolve_ref_deps <- function(deps, remotes, extra) {
  deps <- deps_from_desc(deps, last = FALSE)

  parse <- function(x) {
    str_trim(strsplit(x, "\\s*,\\s*", perl = TRUE)[[1]])
  }

  check_names <- function(x) {
    nms <- vcapply(x, function(e) e$package %||% NA_character_)
    bad <- is.na(nms)
    if (any(bad)) {
      stop(
        "Cannot determine package name for remote(s): ",
        paste(vcapply(x, "[[", "ref")[bad], collapse = ", ")
      )
    }
    nms
  }

  if (!is.na(remotes)) {
    remotes <- str_trim(na.omit(remotes))
    remotes <- parse(remotes)
    remotes_packages <- check_names(parse_pkg_refs(remotes))
    keep <- which(remotes_packages %in% deps$package)
    deps$ref[match(remotes_packages[keep], deps$package)] <- remotes[keep]
  }

  if (length(extra) > 0) {
    xdeps <- parse_all_deps(extra)
    xdeps$package <- check_names(parse_pkg_refs(xdeps$package))
    deps <- rbind(deps, xdeps)
  }

  deps
}

#' Shorthands for dependency specifications
#'
#' @details
#' Supports concise ways of specifying which types of dependencies of
#' a package should be installed. It is similar to how
#' [utils::install.packages()] interprets its `dependencies` argument.
#' Possible values for the `deps` argument are:
#' - `TRUE`: This means all hard dependencies plus `Suggests` for
#'   direct installations, and hard dependencies only for dependent
#'   packages.
#' - `FALSE`: no dependencies are installed at all.
#' - `NA` (any atomic type, so `NA_character_`, etc. as well): only hard
#'   dependencies are installed. See [pkg_dep_types_hard()].
#' - If a list with two entries named `direct` and `indirect`, it is taken
#'   as the requested dependency types, for direct installations and
#'   dependent packages.
#' - If a character vector, then it is taken as the dependency types
#'   for direct installations, and the hard dependencies are
#'   used for the dependent packages.
#'
#' If `"hard"` is included, then it is replaced by the hard dependency
#' types. If `"soft"` or `"all"` is included, then it is replaced by all
#' dependency types.
#'
#' ## Extra dependencies
#'
#' pkgdepends supports extra dependency types for direct installations.
#' These are specified with a `Config/Needs/` prefix in `DESCRIPTION`
#' and they can contain package references, separated by commas.
#' For example you can specify packages that are only needed for the
#' pkgdown website of the package:
#'
#' ```
#' Config/Needs/website: r-lib/pkgdown
#' ```
#'
#' @param deps See below.
#' @return A named list with two character vectors: `direct`, `indirect`,
#' the dependency types to use for direct installations and dependent
#' packages.
#'
#' @family package dependency utilities
#' @export

as_pkg_dependencies <- function(deps) {
  assert_that(is_dependencies(deps))

  hard <- pkg_dep_types_hard()
  soft <- pkg_dep_types()

  res <- if (isTRUE(deps)) {
    list(c(hard, "Suggests"), hard)

  } else if (identical(deps, FALSE)) {
    list(character(), character())

  } else if (is_na_scalar(deps)) {
    list(hard, hard)

  } else if (is.list(deps) && all(names(deps) == c("direct", "indirect"))) {
    deps

  } else {
    list(deps, hard)
  }

  res <- lapply(res, function(x) {
    if ("hard" %in% x) {
      x <- unique(c(hard, setdiff(x, "hard")))
    }
    if (any(c("soft", "all") %in% x)) {
      x <- unique(c(soft, setdiff(x, c("all", "soft"))))
    }
    x
  })

  names(res) <- c("direct", "indirect")
  res
}

extra_config_fields <- function(x) {
  grep("^config/needs/", x, value = TRUE, ignore.case = TRUE)
}
