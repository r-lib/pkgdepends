
# There is no easy way to update the database in yum/dnf/zypper.
# But FIXME. We'll need to solve this issue when (and if) we'll check the
# installed system packages, and only install/update the ones that are
# missing or out of date. For now this is OK, the first `install` command
# will update the DB (cache it is called I believe), and the rest will
# use it.

sysreqs2_cmds <- utils::read.table(
   stringsAsFactors = FALSE, header = TRUE, textConnection("
   os         os_release  update              install
   ubuntu     *           'apt-get -y update' 'apt-get -y install'
   debian     *           'apt-get -y update' 'apt-get -y install'
   centos     *           NA                  'yum install -y'
   rockylinux *           NA                  'dnf install -y'
   redhat     6           NA                  'yum install -y'
   redhat     7           NA                  'yum install -y'
   redhat     *           NA                  'dnf install -y'
   fedora     *           NA                  'dnf install -y'
   opensuse   *           NA                  'zypper --non-interactive install'
   sle        *           NA                  'zypper --non-interactive install'
"))

sysreqs2_is_supported <- function(os, os_release) {
  os %in% sysreqs2_cmds$os
}

sysreqs2_command <- function(os, os_release,
                             cmd = c("install", "update")) {
  cmd <- match.arg(cmd)
  sel <- which(sysreqs2_cmds$os == os & sysreqs2_cmds$os_release == os_release)[1]
  if (is.na(sel)) {
    sel <- which(sysreqs2_cmds$os == os & sysreqs2_cmds$os_release == "*")[1]
  }
  if (is.na(sel)) {
    stop(
      "Unknown OS. Don't know how to install system packages for ",
      os,
      " ",
      os_release
    )
  }

  sysreqs2_cmds[[cmd]][sel]
}

sysreqs2_resolve <- function(sysreqs, os = NULL, os_release = NULL,
                             config = NULL, ...) {
  synchronize(sysreqs2_async_resolve(sysreqs, os, os_release, config, ...))
}

sysreqs2_async_resolve <- function(sysreqs, os, os_release, config, ...) {
  sysreqs; os; os_release; config; list(...)
  start <- Sys.time()

  if (is.null(os) || is.null(os_release)) {
    lnx <- detect_linux()
    os <- os %||% lnx$distribution
    os_release <- os_release %||% lnx$release
  }
  config <- config %||% current_config()

  sysreqs2_async_update_metadata(config = config)$
    then(function() {
      sysreqs2_match(sysreqs, os = os, os_release = os_release, config = config, ...)
    })$
    then(function(recs) {
      upd <- sysreqs2_command(os, os_release, "update")
      pre <- unlist(lapply(recs, "[[", "pre_install"))
      post <- unlist(lapply(recs, "[[", "post_install"))
      if (is.na(upd)) upd <- character()
      cmd <- sysreqs2_command(os, os_release, "install")
      pkgs <- unlist(lapply(recs, "[[", "packages"))
      pkgs <- if (length(pkgs)) paste(pkgs, collapse = " ") else character()
      pkgs <- if (length(pkgs)) paste(cmd, pkgs)
      # no need to update if nothing to do
      if (length(pre) + length(pkgs) + length(post) == 0) upd <- character()
      list(
        os = os,
        os_release = os_release,
        url = NA_character_,
        total = c(total = as.double(Sys.time() - start, units = "secs")),
        pre_install = c(upd, pre),
        install_scripts = pkgs,
        post_install = post,
        records = recs
      )
    })
}

sysreqs2_git_repo <- function() {
  list(
    repo = Sys.getenv(
      "R_PKG_SYSREQS_GIT_REPO_URL",
      "https://github.com/r-hub/r-system-requirements.git"
    ),
    ref = Sys.getenv(
      "R_PKG_SYSREQS_GIT_REPO_REF",
      "HEAD"
    )
  )
}

sysreqs2_update_metadata <- function(path = NULL, config = NULL) {
  synchronize(sysreqs2_async_update_metadata(path = path, config = config))
}

sysreqs2_async_update_metadata <- function(path = NULL, config = NULL) {
  path <- path %||% file.path(get_user_cache_dir()$root, "sysreqs")
  config <- config %||% current_config()
  head_file <- file.path(path, "HEAD")

  if (file.exists(head_file)) {
    mt <- file.mtime(head_file)
    upd <- config$get("metadata_update_after")
    if (!is.na(mt) && Sys.time() - mt < upd) return(async_constant())
  }

  head <- if (file.exists(head_file)) readLines(head_file)[1] else ""
  repo <- sysreqs2_git_repo()
  async_git_list_refs_v1(repo$repo)$
    then(function(refs) {
      rem_head <- refs$refs$hash[refs$refs$ref == "HEAD"]
      if (rem_head == head) return()
      tmp <- paste0(path, "-new")
      async_git_download_repo(repo$repo, rem_head, tmp)$
        then(function() {
          unlink(path, recursive = TRUE, force = TRUE)
          file.rename(tmp, path)
          writeLines(rem_head, head_file)
        })
    })
}

sysreqs2_match <- function(sysreqs, path = NULL, os = NULL,
                           os_release = NULL, config = NULL ) {
  sysreqs <- paste(sysreqs, collapse = " ")
  path <- path %||% file.path(get_user_cache_dir()$root, "sysreqs")
  rules <- dir(file.path(path, "rules"), pattern = "[.]json$", full.names = TRUE)

  records <- list()

  for (r in rules) {
    rule <- fromJSON(r, simplifyVector = FALSE)
    pats <- unlist(rule$patterns)
    if (!any(vlapply(pats, grepl, sysreqs, ignore.case = TRUE))) next
    for (dep in rule$dependencies) {
      appl <- FALSE
      for (const in dep$constraints) {
        if (const$distribution == os &&
            (is.null(const$versions) || os_release %in% const$versions)) {
          appl <- TRUE
          break
        }
      }
      if (appl) {
        records[[length(records) + 1]] <- list(
          packages = unname(unlist(dep$packages)),
          pre_install = unname(unlist(dep$pre_install)),
          post_install = unname(unlist(dep$post_install))
        )
        break
      }
    }
  }

  records
}
