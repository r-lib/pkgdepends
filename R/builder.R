
pkg_build <- function(pkg, library = .libPaths()[1]) {
  pkgdir <- file.path(library, pkg)
  if (!dir.exists(pkgdir)) {
    throw(pkg_error(
      "Cannot find package {.pkg {pkg}} in library at {.path {library}}."
    ))
  }
  version <- desc::desc_get_field("Version", file = pkgdir)
  rversion <- get_minor_r_version(getRversion())
  platform <- pkgcache::current_r_platform()

  sys <- sysname()
  if (sys == "windows") {
    install_md5_sums(pkg)
    fn <- paste0(pkg, "_", version, "_R", rversion, ".zip")
    zip::zip(fn, pkgdir, mode = "cherry-pick")

  } else {
    ext <- if (sys == "mac") ".tgz" else ".tar.gz"
    fn <- paste0(pkg, "_", version, "_", "R", rversion, "_", platform, ext)
    ffn <- file.path(normalizePath("."), fn)
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(dirname(pkgdir))
    utils::tar(ffn, pkg, compression = "gzip", compression_level = 9)
  }

  fn
}

install_md5_sums <- function(pkgdir) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)

  setwd(pkgdir)
  fns <- setdiff(dir(".", recursive = TRUE), "MD5")
  md5 <- cli::hash_file_md5(fns)
  writeLines(
    paste0(md5, " *", fns),
    "MD5"
  )
}
