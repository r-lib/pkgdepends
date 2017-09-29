
dependency_fields <- function() {
  c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")
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
  base <- c("R", base_packages())
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
  base <- c("R", base_packages())
  res <- as_tibble(deps[deps$type %in% dependencies & !deps$package %in% base,
                        c("ref", "type", "package", "op", "version")])
  rownames(res) <- NULL
  res
}

get_cran_deps <- function(package, version, data, dependencies) {

  ## Some dependency types might not be present here
  dependencies <- intersect(dependencies, colnames(data))
  
  wh <- if (version == "") {
    wh <- which(data[ , "Package"] == package)
  } else {
    wh <- which(data[ , "Package"] == package &
                  data[, "Version"] == version)
  }
  wh <- wh[1]
  version <- data[wh, "Version"]
  
  deps <- na.omit(unlist(data[wh, dependencies]))
  res <- do.call(rbind, parse_deps(deps, names(deps)))
  res$ref <- res$package
  res <- res[, c("ref", setdiff(names(res), "ref"))]
  
  ## TODO: Bioc? Additional repositories?
  res
}
