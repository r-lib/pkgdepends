
read_etag <- function(etag_file) {
  if (file.exists(etag_file)) {
    tryCatch(readLines(etag_file)[1], error = function(e) NA)
  } else {
    NA
  }
}

#' @importFrom curl parse_headers_list

download_file <- function(url, target, etag_file = NULL) {
  url; target; etag_file
  target <- normalizePath(target, mustWork = FALSE)
  tmp_target <- paste0(target, ".tmp")
  http_get(url, file = tmp_target)$then(function(resp) {
    file.rename(tmp_target, target)
    if (!is.null(etag_file)) {
      etag <- parse_headers_list(resp$headers)[["etag"]]
      writeLines(etag, etag_file)
    }
    resp$status_code
  })
}

download_if_newer <- function(url, target, etag_file = NULL) {
  force(url) ; force(target)

  headers <- character()
  if (!is.null(etag_file)) {
    etag_old <- read_etag(etag_file)
    if (file.exists(target) && !is.na(etag_old)) {
      headers <- c(headers, c("If-None-Match" = etag_old))
    }
  }

  target <- normalizePath(target, mustWork = FALSE)
  tmp_target <- paste(target, ".tmp")

  http_get(url, file = tmp_target, headers = headers)$then(function(resp) {
    if (resp$status_code == 304) {
      ## Current, nothing to do
    } else if (resp$status_code == 200) {
      file.rename(tmp_target, target)
      etag <- parse_headers_list(resp$headers)[["etag"]]
      if (!is.null(etag_file)) writeLines(etag, etag_file)
    } else {
      stop(paste("HTTP error: ", resp$status_code))
    }
    unlink(tmp_target)

    resp
  })
}

download_try_list <- function(urls, targets, etag_file = NULL,
                              headers = character()) {
  assert_that(is.character(urls), length(urls) >= 1)

  force(urls) ; force(targets) ; force(etag_file) ; force(headers)

  if (!is.null(etag_file)) {
    etag_old <- read_etag(etag_file)
    if (any(file.exists(targets)) && !is.na(etag_old)) {
      headers <- c(headers, c("If-None-Match" = etag_old))
    }
  }

  target <- normalizePath(targets[1], mustWork = FALSE)
  tmp_target <- paste(target, ".tmp")

  status_code <- NULL
  errors <- NULL
  async_detect(
    urls,
    function(x) {
      force(x)
      http_get(x, file = tmp_target, headers = headers)$
        then(function(resp) {
          http_stop_for_status(resp)
          if (resp$status_code == "304") {
            ## Current, nothing to do
          } else {
            file.rename(tmp_target, targets[1])
            etag <- parse_headers_list(resp$headers)[["etag"]]
            if (!is.null(etag_file)) writeLines(etag, etag_file)
          }
          unlink(tmp_target)
          status_code <<- resp$status_code
          TRUE
        })$
        catch(function(err) {
          errors <<- c(errors, structure(list(err), names = x))
        })
    },
    .limit = 1
  )$then(function(url) {
    if (is.null(url)) {
      stop(make_error("All URLs failed", "http_error", error = errors))
    }
    status_code
  })

}

get_async_value <- function(x) {
  if (is_deferred(x)) x$get_value() else x
}
