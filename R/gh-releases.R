
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
      browser()
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
