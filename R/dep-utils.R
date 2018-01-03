
#' @importFrom tibble tibble
#' @importFrom rematch2 re_match

fast_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), deptypes())
  ## as.character is for empty tibbles, e.g. from empty BioC repos
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
    parsed <- parsed[order(parsed$idx), ]

  } else {
    parsed <- tibble(upstream = character(),
                     idx = integer(),
                     ref = character(),
                     type = character(),
                     package = character(),
                     version = character(),
                     op = character())
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
  tibble(ref = character(), type = character(), package = character(),
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

deps_from_desc <- function(deps, dependencies, last) {
  op_ver <- strsplit(deps$version, "\\s+")
  deps$op <- vcapply(op_ver, "[", 1)
  deps$op[deps$op == "*"] <- ""
  deps$version <- vcapply(op_ver, "[", 2)
  deps$version[is.na(deps$version)] <- ""
  deps$ref <- paste0(deps$package, if (last) "@last")
  base <- base_packages()
  res <- as_tibble(deps[deps$type %in% dependencies & !deps$package %in% base,
                        c("ref", "type", "package", "op", "version")])
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

get_cran_extension <- function(platform) {
  switch(
    platform,
    "source" = ".tar.gz",
    "macos" = ".tgz",
    "windows" = ".zip",
    stop("Unknown platform: ", sQuote(platform))
  )
}

resolve_ref_deps <- function(deps, remotes, dependencies) {
  deps <- deps_from_desc(deps, dependencies, last = FALSE)

  if (is.na(remotes)) return (deps)

  parse <- function(x) {
    str_trim(strsplit(x, "\\s*,\\s*", perl = TRUE)[[1]])
  }

  remotes <- str_trim(na.omit(remotes))
  remotes <- parse(remotes)
  remotes_packages <- vcapply(parse_remotes(remotes), "[[", "package")
  keep <- which(remotes_packages %in% deps$package)
  deps$ref[match(remotes_packages[keep], deps$package)] <- remotes[keep]
  deps
}
