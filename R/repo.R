
repo_list <- function(..., path = ".") {
  pkgs <- suppressWarnings(
    pkgcache::parse_packages(file.path(path, "PACKAGES"))
  )
  if (nrow(pkgs) == 0) {
    pkgs$Package <- character()
    pkgs$Version <- character()
    pkgs$Depends <- character()
    pkgs$Imports <- character()
    pkgs$Suggests <- character()
    pkgs$Enhances <- character()
    pkgs$LinkingTo <- character()
    pkgs$License <- character()
    pkgs$File <- character()
    pkgs$DownloadURL <- character()
    pkgs$OS <- character()
    pkgs$Arch <- character()
    pkgs$Built <- character()
    pkgs$Filesize <- character()
    pkgs$SHA256 <- character()
    pkgs$RVersion <- character()
    pkgs$Platform <- character()
  }

  pkgs
}

repo_delete <- function(package, ..., path = ".") {
  pkgs <- repo_list(path = path)
  idx <- find_in_data_frame(pkgs, package = package, ...)
  if (length(idx)) {
    pkgs <- pkgs[-idx, , drop = FALSE]
  }
  PACKAGES <- file.path(path, "PACKAGES")
  write_dcf(pkgs, PACKAGES)
}

repo_add <- function(file, ..., path = ".") {
  pkgs <- repo_list(path = path)
  pkg_data <- get_package_data(file)
  pkgs <- rbind_expand(pkgs, pkg_data)
  PACKAGES <- file.path(path, "PACKAGES")
  write_dcf(pkgs, PACKAGES)
}

get_desc <- function(path) {
  pkg <- sub("_.*$", "", basename(path))
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  untar(path, files = paste0(pkg, "/DESCRIPTION"), exdir = tmp)
  desc::desc(file.path(tmp, pkg))
}

get_package_data <- function(path) {
  desc <- get_desc(path)
  pkgname <- desc$get_field("Package")
  pkgver <- desc$get_field("Version")
  built <- desc$get_built()
  rminor <- get_minor_r_version(built$R)
  os <- canonize_os(built$Platform)
  arch <- canonize_arch(built$Platform)
  deps <- unname(desc$get("Depends"))
  pkg <- data_frame(
    Package = pkgname,
    Version = pkgver,
    Depends = paste0(
      if (!is.na(deps)) paste0(deps, ", "),
      "R (>= ", rminor, "), R (< ", rminor, ".99)"
    ),
    Imports = unname(desc$get("Imports")),
    Suggests = unname(desc$get("Suggests")),
    Enhances = unname(desc$get("Enhances")),
    LinkingTo = unname(desc$get("LinkingTo")),
    License = unname(desc$get("License")),
    File = basename(path),
    DownloadURL = paste0(
      "https://github.com/cran/",
      pkgname,
      "/releases/download/",
      pkgver,
      "/",
      basename(path)
    ),
    OS = os,
    Arch = arch,
    Built = desc$get_field("Built"),
    Filesize = file.size(path),
    SHA256 = cli::hash_file_sha256(path),
    RVersion = rminor,
    Platform = unname(desc$get("RemoteBuildPlatform"))
  )

  pkg
}

# Remove packages with
# - the same package name
# - the same R version
# - the same OS (or no OS)
# - the same arch (or no arch)

repo_update <- function(file, ..., path = ".") {
  pkgs <- repo_list(path = path)
  pkg_data <- get_package_data(file)

  idx <- which(
    pkgs$Package == pkg_data$Package &
    pkgs$RVersion == pkg_data$RVersion &
    (is.na(pkg_data$OS) | is.na(pkgs$OS) | identical(pkgs$OS, pkg_data$OS)) &
    (is.na(pkg_data$Arch) | is.na(pkgs$Arch) | identical(pkgs$Arch, pkg_data$Arch))
  )
  if (length(idx)) {
    pkgs <- pkgs[-idx, , drop = FALSE]
  }

  pkgs <- rbind_expand(pkgs, pkg_data)
  PACKAGES <- file.path(path, "PACKAGES")
  write_dcf(pkgs, PACKAGES)
}

write_dcf <- function(meta, PACKAGES, quiet = FALSE) {
  if (!quiet) cat("Writing ", PACKAGES, "\n")
  meta <- as.matrix(meta)
  write.dcf(meta, PACKAGES, width = 200)
  con <- gzfile(paste0(PACKAGES, ".gz"), "wt")
  write.dcf(meta, con, width = 200)
  close(con)
  saveRDS(meta, paste0(PACKAGES, ".rds"), compress = "xz", version = 2)
  invisible()
}

# case insensitive!

find_in_data_frame <- function(df, ..., .list = NULL) {
  cols <- drop_nulls(c(list(...), .list))
  idx <- seq_len(nrow(df))
  for (i in seq_along(cols)) {
    if (length(idx) == 0) break
    n <- tolower(names(cols)[i])
    idx <- idx[df[[n]][idx] %in% cols[[i]]]
  }

  idx
}

canonize_arch <- function(platform) {
  if (platform == "") return(NA_character_)
  arch <- strsplit(platform, "-", fixed = TRUE)[[1]][1]
  c("aarch64" = "arm64", "x86_64" = "amd64")[[arch]]
}

canonize_os <- function(platform) {
  if (platform == "") return(NA_character_)
  os <- strsplit(platform, "-", fixed = TRUE)[[1]][3]
  if (substr(os, 1, 6) == "darwin") os <- "darwin"
  if (substr(os, 1, 5) == "mingw") os <- "windows"
  if (substr(os, 1, 7) == "solaris") os <- "solaris"
  os
}

git <- function (..., echo_cmd = TRUE, echo = TRUE, dry_run = FALSE,
                 stderr_to_stdout = FALSE) {
  if (dry_run) {
    cat("git", c(...), "\n")
  } else {
    processx::run("git", c(...), echo_cmd = echo_cmd, echo = echo,
                  stderr_to_stdout = stderr_to_stdout)
  }
}

git_push <- function() {
  git("add", "-A", ".")
  git("commit", "-m", "Update package")
  git("push", "--porcelain", "origin", stderr_to_stdout = TRUE)
}

update_gh_repo <- function(repo, subdir, file) {
  file <- normalizePath(file)

  oldwd <- getwd()
  workdir <- tempfile()
  on.exit({ setwd(oldwd); unlink(workdir, recursive = TRUE) }, add = TRUE)
  dir.create(workdir)
  setwd(workdir)

  git("clone", paste0("https://github.com/", repo))
  prepo <- parse_slug(repo)
  setwd(prepo$repo)
  setwd(subdir)

  git("config", "credential.helper", "cache")
  gitcreds::gitcreds_approve(list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = Sys.getenv("GITHUB_PAT")
  ))

  repeat {
    git("pull")
    repo_update(file)
    tryCatch({
      git_push()
      break
    }, error = function(err) {
      if (!grepl("(non-fast-forward|fetch first|cannot lock ref)",
                 err$stderr)) {
        stop(err)
      }
      git("reset", "HEAD^")
      git("checkout", "--", ".")
    })
  }

  invisible()
}