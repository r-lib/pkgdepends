
ghr <- local({

  # -------------------------------------------------------------------------

  ghr_list <- function(repo) {
    synchronize(async_ghr_list(repo))
  }

  async_ghr_list <- function(repo) {
    repo <- parse_slug(repo)
    query <- glue("{
    rateLimit {
      cost
      remaining
    }
    repository(owner: \"<repo$user>\", name: \"<repo$repo>\") {
      releases(last: 100) {
        nodes {
          id
          name
          createdAt
          tagName
        }
      }
    }
  }", .open = "<", .close = ">")

    github_query(query)$
      then(function(resp) {
        rls <- resp$obj$data$repository$releases$nodes
        data_frame(
          id = vcapply(rls, "[[", "id"),
          name = vcapply(rls, "[[", "name"),
          tag_name = vcapply(rls, "[[", "tagName"),
          created_at = parse_iso_8601(vcapply(rls, "[[", "createdAt"))
        )
      })
  }

  # -------------------------------------------------------------------------

  ghr_get <- function(repo, tag) {
    synchronize(async_ghr_get(repo, tag))
  }

  async_ghr_get <- function(repo, tag) {
    prepo <- parse_slug(repo)
    ep <- glue("/repos/{prepo$owner}/{prepo$repo}/releases/tags/{tag}")
    async_github_v3_query(ep)$
      then(function(resp) {
        jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      })
  }

  # -------------------------------------------------------------------------

  ghr_list_assets <- function(repo, tag) {
    synchronize(async_ghr_list_assets(repo, tag))
  }

  async_ghr_list_assets <- function(repo, tag) {

    repo <- parse_slug(repo)
    query <- glue("{
    rateLimit {
      cost
      remaining
    }
    repository(owner: \"<repo$user>\", name: \"<repo$repo>\") {
      release(tagName: \"<tag>\") {
        releaseAssets(last: 100) {
          nodes {
            id
            name
            downloadUrl
            size
            createdAt
            updatedAt
            contentType
          }
        }
      }
    }
  }", .open = "<", .close = ">")

    github_query(query)$
      then(function(resp) {
        asts <- resp$obj$data$repository$release$releaseAssets$nodes
        data_frame(
          id = vcapply(asts, "[[", "id"),
          name = vcapply(asts, "[[", "name"),
          download_url = vcapply(asts, "[[", "downloadUrl"),
          size = viapply(asts, "[[", "size"),
          created_at = parse_iso_8601(vcapply(asts, "[[", "createdAt")),
          updated_at = parse_iso_8601(vcapply(asts, "[[", "updatedAt")),
          content_type = vcapply(asts, "[[", "contentType")
        )
      })
  }

  # -------------------------------------------------------------------------

  ghr_add_asset <- function(repo, file, tag, name = basename(file)) {
    invisible(synchronize(async_ghr_add_asset(repo, file, tag, name)))
  }

  async_ghr_add_asset <- function(repo, file, tag,
                                  name = basename(file)) {

    repo; file; tag; name

    async_ghr_get(repo, tag)$
      catch(async_http_404 = function(err) {
        async_ghr_create(repo, tag)
      })$
        then(function(rel) rel$upload_url)$
         then(function(upload_url) {
           upload_url <- sub("[{].*[}]", "", upload_url)
           prepo <- parse_slug(repo)
           async_github_v3_query(
             url = upload_url,
             endpoint = "",
             query = c(name = name),
             method = "POST",
             headers = c("Content-Type" = "application/octet-stream"),
             file = file
           )
         })$
           then(function(resp) {
             jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
           })
  }

  # -------------------------------------------------------------------------

  ghr_create <- function(repo, tag) {
    invisible(synchronize(async_ghr_create(repo, tag)))
  }

  async_ghr_create <- function(repo, tag, description = "", draft = FALSE,
                               prerelease = FALSE,
                               generate_release_notes = FALSE) {
    prepo <- parse_slug(repo)
    ep <- glue("/repos/{prepo$owner}/{prepo$repo}/releases")
    data <- toJSON(list(
      tag_name = tag,
      name = paste0(prepo$repo, " ", tag),
      body = description,
      draft = draft,
      prerelease = prerelease,
      generate_release_notes = generate_release_notes
    ), auto_unbox = TRUE)

    async_github_v3_query(
      ep,
      data = data,
      method = "POST"
    )$
      then(function(resp) {
        jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
      })
  }

  # -------------------------------------------------------------------------

  parse_slug <- function(slug) {
    parts <- strsplit(slug, "/", fixed = TRUE)[[1]]
    list(user = parts[1], owner = parts[1], repo = parts[2])
  }

  async_github_v3_query <- function(endpoint, query = NULL,
                                    method = c("GET", "POST"),
                                    headers = NULL,
                                    data = NULL,
                                    file = NULL,
                                    url = NULL) {
    method <- match.arg(method)

    headers <- update_named_vector(type_github_get_headers(), headers)

    base <- url %||% Sys.getenv("R_PKG_GITHUB_API_URL", "https://api.github.com")
    query_str <- paste(
      glue("{curl::curl_escape(names(query))}={curl::curl_escape(query)}"),
      collapse = "&"
    )
    url <- paste0(base, endpoint, "?", query_str)

    px <- if (method == "GET") {
            http_get(url, headers = headers)

          } else if (method == "POST") {
            if (is.null(data) + is.null(file) != 1) {
              throw(pkg_error(
                "Must specify exactly of {.arg data} and {.arg file} for POST requests."
              ))
            }
            if (is.null(data)) data <- readBin(file, "raw", file.size(file))
            http_post(url, data, headers = headers)
          }

    px$
      then(http_stop_for_status)
  }

  # github_query() is in type-github.R

  # -----------------------------------------------------------------------
  # Exported functions

  structure(
    list(
      .internal         = environment(),

      async_add_asset   = async_ghr_add_asset,
      async_create      = async_ghr_create,
      async_get         = async_ghr_get,
      async_list        = async_ghr_list,
      async_list_assets = async_ghr_list_assets,

      add_asset         = ghr_add_asset,
      create            = ghr_create,
      get               = ghr_get,
      list              = ghr_list,
      list_assets       = ghr_list_assets
    )
  )
})

# -------------------------------------------------------------------------
# Docs

#' List GitHub releases
#'
#' Lists the last 100 releases for a GitHub repository.
#'
#' `ghr$async_list()` is the async version of `ghr$list()`.
#'
#' @name ghr$list
#' @keywords internal
#' @param repo Repository slug, e.g. `"cran/cli"`.
#' @return Data frame with columns:
#'   * `id`: release id,
#'   * `name`: release name, usuaally the version number, possibly with
#'     a `v` prefix: `3.6.1` or `v3.6.1`, but can be different.
#'   * `tag_name`: usually the same as `name`.
#'   * `created_at`: `POSIXct` vector.

ghr$list

#' Get information about a release, including release assets
#'
#' `ghr$async_get` is the async version of `ghr$get`.
#'
#' @name ghr$get
#' @keywords internal
#' @param repo Repository slug, e.g. `"cran/cli"`.
#' @param tag Tag to get.
#' @return Named list, see
#'   <https://docs.github.com/en/rest/releases/releases#get-a-release>
#'   for the entries.

ghr$get

#' List assets of a release
#'
#' `ghr$async_list_assets()` is the async version of `ghr$list_assets()`
#'
#' @name ghr$list_assets
#' @keywords internal
#' @usage ghr$list_assets(repo, tag)
#' @param repo Repository slug, e.g. `"cran/cli"`.
#' @param tag Tag to query.
#' @return Data frame with columns:
#'   * `id`: asset id,
#'   * `name`: file name of the asset,
#'   * `download_url`: download URL,
#'   * `size`: size in bytes,
#'   * `created_at`: `POSIXct` vector,
#'   * `updated_at`: `POSXct` vector,
#'   * `content_type`: content type of asset.

ghr$list_assets

#' Add asset got GitHub release
#'
#' `ghr$async_add_asset()` is the async version of `ghr$add_asset()`.
#'
#' @name ghr$add_asset
#' @keywords internal
#' @usage ghr%add_asset(repo, file, tag, name = basename(file))
#' @param repo Repository slug, e.g. `cran/cli`.
#' @param file Path to file to upload as an asset.
#' @param tag Tag name to add the asset to. It must exist on GitHub.
#' @param name File name of the asset in the release.
#' @return Response from GitHub as a named list. See
#'   <https://docs.github.com/en/rest/releases/assets#upload-a-release-asset>
#'   for the structure.

ghr$add_asset

#' Create a GitHub release
#'
#' `ghr$async_create()` is an async version of `ghr$create()`.
#'
#' @name ghr$create
#' @keywords internal
#' @usage
#' ghr$create(
#'   repo,
#'   tag,
#'   description = "",
#'   draft = FALSE,
#'   prerelease = FALSE,
#'   generage_release_notes = FALSE
#' )
#' @param repo Repository slug, e.g. `cran/cli`.
#' @param tag Tag name to create a release for. It must exist on GitHub.
#' @param description Release description.
#' @param draft Whether to create a draft release.
#' @param prerelease Whether to create a prerelease.
#' @param generate_release_notes Whether to auto-generate release notes.
#' @return Response from GitHub as a named list. See
#'   <https://docs.github.com/en/rest/releases/releases#create-a-release>
#'   for the structure.

ghr$create
