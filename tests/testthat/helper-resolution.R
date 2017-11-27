
make_fake_deps <- function(...) {
  assertthat::assert_that(all_named(list(...)))

  d <- desc::desc("!new")
  if (length(list(...))) d$set(...)
  resolve_ref_deps(
    d$get_deps(),
    d$get("Remotes")[[1]],
    remotes_default_config()$dependencies)
}

make_fake_resolution1 <- function(ref, args) {
  pref <- parse_remotes(ref)[[1]]

  mirror <- args$mirror %||% remotes_default_config()$`cran-mirror`
  repodir <- args$repodir %||% "src/contrib"
  version <- args$version %||% "1.0.0"
  filename <- paste0(pref$package, "_", version, ".tar.gz")

  list(
    files = list(list(
      source = c(
        sprintf("%s/%s/%s", mirror, repodir, filename),
        sprintf("%s/%s/Archive/%s/%s", mirror, repodir, pref$pakcvage,
                filename)
      ),
      target = sprintf("src/contrib/%s_%s.tar.gz", pref$package, version),
      platform = args$platform %||% "source",
      rversion = args$rversion %||% "*",
      dir = repodir,
      package = pref$package,
      version = version,
      deps = args$deps %||% make_fake_deps(),
      status = args$status %||% "OK"
    )),
    remote = pref,
    status = args$status %||% "OK"
  )
}

make_fake_resolution <- function(...) {
  pkgs <- list(...)
  assertthat::assert_that(all_named(pkgs))
  ress <- lapply_with_names(
    names(pkgs), function(n) make_fake_resolution1(n, pkgs[[n]]))

  direct <- vlapply(lapply(pkgs, "[[", "direct"), isTRUE)
  remotes_i_resolution_to_df(
    ress,
    parse_remotes(names(pkgs)[direct]),
    remotes_default_config()$cache_dir)
}
