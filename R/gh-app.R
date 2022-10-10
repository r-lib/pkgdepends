
str_starts_with <- function(x, pre) {
  substring(x, 1, nchar(pre)) == pre
}

match_graphql_query <- function(q) {
  re_ref <- paste0(
    "owner: \"(?<user>[^\"]+)\"", "(?s:.)*",
    "name: \"(?<repo>[^\"]+)\"", "(?s:.)*",
    "description: object[(]expression: \"(?<subdir>[^\"]+)\"", "(?s:.)*",
    "sha: object[(]expression: \"(?<ref>[^\"]+)\""
  )

  re_pull_1 <- paste0(
    "owner: \"(?<user>[^\"]+)\"", "(?s:.)*",
    "name: \"(?<repo>[^\"]+)\"", "(?s:.)*",
    "number: (?<pull>[0-9]+)[)]"
  )

  re_pull_2 <- paste0(
    "owner: \"(?<user>[^\"]+)\"", "(?s:.)*",
    "name: \"(?<repo>[^\"]+)\"", "(?s:.)*",
    "description: object[(]expression: \"(?<subdir>[^\"]+)\""
  )

  mch <- re_match(q, re_ref)
  if (!is.na(mch$.match)) {
    mch$subdir <- sub("^[^:]+:(.*)/?DESCRIPTION$", "\\1", mch$subdir)
    return(c(mch, query = "ref"))
  }

  mch <- re_match(q, re_pull_1)
  if (!is.na(mch$.match)) return(c(mch, query = "pull-1"))

  mch <- re_match(q, re_pull_2)
  if (!is.na(mch$.match)) {
    mch$subdir <- sub("^[^:]+:(.*)/?DESCRIPTION$", "\\1", mch$subdir)
    return(c(mch, query = "pull-2"))
  }

  stop("Unknown graphql query")
}

gr_response_headers_graphql <- function(upd = NULL) {
  list(
    server = "GitHub.com",
    `content-type` = "application/json; charset=utf-8",
    `x-oauth-scopes` = "delete:packages, delete_repo, read:org, repo, workflow, write:packages",
    `x-accepted-oauth-scopes` = "repo",
    `x-github-media-type` = "github.v3; format=json",
    `x-ratelimit-limit` = "5000",
    `x-ratelimit-remaining` = "4998",
    `x-ratelimit-reset` = as.integer(Sys.time() + as.difftime(1, units = "hours")),
    `x-ratelimit-used` = "2",
    `x-ratelimit-resource` = "graphql",
    `access-control-expose-headers` = "ETag, Link, Location, Retry-After, X-GitHub-OTP, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Used, X-RateLimit-Resource, X-RateLimit-Reset, X-OAuth-Scopes, X-Accepted-OAuth-Scopes, X-Poll-Interval, X-GitHub-Media-Type, X-GitHub-SSO, X-GitHub-Request-Id, Deprecation, Sunset",
    `access-control-allow-origin` = "*",
    `strict-transport-security` = "max-age=31536000; includeSubdomains; preload",
    `x-frame-options` = "deny",
    `x-content-type-options` = "nosniff",
    `x-xss-protection` = "0",
    `referrer-policy` = "origin-when-cross-origin, strict-origin-when-cross-origin",
    `content-security-policy` = "default-src 'none'",
    vary = "Accept-Encoding, Accept, X-Requested-With",
    `x-github-request-id` = basename(tempfile())
  )
}

gh_complete_repos <- function(repos) {
  if (!"package" %in% names(repos)) repos$package <- repos$repo
  repos$package[is.na(repos$package)] <- repos$repo[is.na(repos$package)]

  if (!"ref" %in% names(repos)) repos$ref <- rep("HEAD", nrow(repos))
  repos$ref[is.na(repos$ref)] <- "HEAD"

  if (!"subdir" %in% names(repos)) repos$subdir <- rep("", nrow(repos))
  repos$subdir[is.na(repos$subdir)] <- ""

  if (!"version" %in% names(repos)) repos$version <- rep("1.0.0", nrow(repos))
  repos$version[is.na(repos$version)] <- "1.0.0"

  if (!"sha" %in% names(repos)) repos$sha <- rep(NA_character_, nrow(repos))
  for (i in seq_len(nrow(repos))) {
    if (!is.na(repos$sha[i])) next
    nc <- nchar(repos$ref[i])
    # complete hash
    if (grepl("^[0-9a-fA-F]*$", repos$ref[i]) && nc == 64) {
      repos$sha[i] <- repos$ref[i]
      next
    }

    # incomplete hash
    if (grepl("^[0-9a-fA-F]*$", repos$ref[i]) && nc < 64) {
      repos$sha[i] <- repos$ref[i]
    } else {
      repos$sha[i] <- ""
    }
    need <- 64 - nchar(repos$sha[i])
    repos$sha[i] <- paste0(
      repos$sha[i],
      paste(sample(c(0:9, letters[1:6]), need, replace = TRUE), collapse = "")
    )
  }

  repos
}

make_dummy_zip <- function(repos, user, repo, sha) {
  package <- repos$package
  mkdirp(tmp <- tempfile())
  withr::local_dir(tmp)
  mkdirp(package)
  file.create(file.path(package, "NAMESPACE"))
  cols <- setdiff(
    names(repos)[!is.na(repos)],
    c("user", "repo", "package", "ref", "subdir", "version", "sha")
  )
  desc <- as.data.frame(repos[cols])
  desc$Package <- desc[["Package"]] %||% repos$package
  desc$Version <- desc[["Version"]] %||% repos$version
  write.dcf(desc, file.path(package, "DESCRIPTION"))
  zip::zip(paste0(package, ".zip"), package)
  file.path(tmp, paste0(package, ".zip"))
}

gh_app <- function(repos = NULL, log = interactive(), options = list()) {

  app <- webfakes::new_app()

  # Log requests by default
  if (log) app$use("logger" = webfakes::mw_log())

  # Parse JSON body, even if no content-type header is sent
  app$use("json body parser" = webfakes::mw_json(
      type = c(
        "",
        "application/json",
        "application/json; charset=utf-8"
      )
  ))

  #  app$use("text body parser" = webfakes::mw_text(type = c("text/plain", "application/json")))
  #  app$use("multipart body parser" = webfakes::mw_multipart())
  #  app$use("URL encoded body parser" = webfakes::mw_urlencoded())

  # Add etags by default
  app$use("add etag" = webfakes::mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", as.character(Sys.time()))
    "next"
  })

  repos <- gh_complete_repos(repos)

  app$locals$repos <- repos
  app$locals$data <- list()

  pkg_match <- function(psd) {
    which(
      app$locals$repos$user   == psd$user &
      app$locals$repos$repo   == psd$repo &
      (app$locals$repos$ref   == psd$ref  |
       str_starts_with(app$locals$repos$sha, psd$ref)) &
      app$locals$repos$subdir == psd$subdir
    )[1]
  }

  pkg_match_sha <- function(psd) {
    which(
      app$locals$repos$user   == psd$user &
      app$locals$repos$repo   == psd$repo &
      app$locals$repos$sha    == psd$sha
    )[1]
  }

  pkg_is_known <- function(psd) {
    !is.na(pkg_match(psd))
  }

  pkg_description <- function(psd) {
    data <- app$locals$repos[pkg_match(psd),]
    paste0(c(
      paste0("Package: ", data$package),
      paste0("Version: ", data$version)
    ), "\n", collapse = "")
  }

  pkg_sha <- function(psd) {
    app$locals$repos$sha[pkg_match(psd)]
  }

  app$post("/graphql", function(req, res) {
    psd <- match_graphql_query(req$json$query)

    add_gh_headers <- function() {
      headers <- gr_response_headers_graphql()
      for (i in seq_along(headers)) {
        res$set_header(names(headers)[i], headers[i])
      }
    }

    if (psd$query == "ref") {
      if (!pkg_is_known(psd)) {
        res$send_status(404)
        return()
      }
      add_gh_headers()
      res$send_json(auto_unbox = TRUE,
        list(data = list(repository = list(
              description = list(
                isBinary = FALSE,
                text = pkg_description(psd)
              ),
              sha = list(oid = pkg_sha(psd))
        ))
      ))

    } else if (psd$query == "pull-1") {
      add_gh_headers()

    } else if (psd$query == "pull-2") {
      add_gh_headers()

    }
  })

  app$get("/repos/:user/:repo/zipball/:sha", function(req, res) {
    wch <- pkg_match_sha(list(
      user = req$params$user,
      repo = req$params$repo,
      sha = req$params$sha
    ))

    if (is.na(wch)) {
      res$send_status(404)
    } else {
      z <- make_dummy_zip(app$locals$repos[wch,])
      res$send_file(z, root = "/")
    }
  })

  app
}
