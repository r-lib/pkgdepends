
#' Download a file, asynchronously
#'
#' This is the asynchronous version of [utils::download.file()].
#'
#' `download_file` also has some nice improvements:
#' * It uses a temporary file, so never leaves a partial file at `destfile`.
#' * It can write the HTTP Etag from the response to a file, which can
#'   be used in [download_if_newer()], etc.
#' * It returns the HTTP response as part of the error message if the
#'   response status code indicates a client or server error.
#' * Well, it is asynchronous.
#'
#' @param url URL to download.
#' @param destfile Destination file.
#' @param etag_file If not `NULL`, and the reponse is successful and
#'   includes an `Etag` header, then this header is stored in this file.
#'   It can be used to cache the file, with the [download_if_newer()] or
#'   the [download_one_of()] functions.
#' @param tmp_destfile Where to store the temporary destination file.
#' @param ... Additional arguments are passed to [async::http_get()].
#' @return An [async::deferred] object. It resolves to a list with entries:
#'   * `url`: The URL in the request.
#'   * `destfile`: The destination file.
#'   * `response`: The response object from the curl package.
#'   * `etag`: The Etag of the response, of `NULL` if missing.
#'   * `etag_file`: The file the Etag was written to, or `NULL` otherwise
#'
#' @family async HTTP tools
#' @importFrom curl parse_headers_list
#' @keywords internal
#' @section Examples:
#' ```
#' dest1 <- tempfile(fileext = ".jpeg")
#' dest2 <- tempfile(fileext = ".png")
#' dl <- function() {
#'   when_all(
#'     download_file("https://httpbin.org/image/jpeg", dest1),
#'     download_file("https://httpbin.org/image/png", dest2)
#'   )
#' }
#' resps <- synchronise(dl())
#' lapply(resps, function(x) x$response$status_code)
#' resps[[1]]$url
#' resps[[1]]$destfile
#' resps[[1]]$response$times
#' file.exists(dest1)
#' file.exists(dest2)
#'
#' ## HTTP errors contain the response
#' dest <- tempfile()
#' err <- tryCatch(
#'   synchronise(download_file("https://httpbin.org/status/418", dest)),
#'   error = function(e) e
#' )
#' err
#' names(err)
#' cat(rawToChar(err$response$content))
#' ```

download_file <- function(url, destfile, etag_file = NULL,
                          tmp_destfile = paste0(destfile, ".tmp"), ...) {
  "!DEBUG downloading `url`"
  assert_that(
    is_string(url),
    is_path(destfile),
    is_path(tmp_destfile),
    is_path_or_null(etag_file))
  force(list(...))

  destfile <- normalizePath(destfile, mustWork = FALSE)
  tmp_destfile <- normalizePath(tmp_destfile, mustWork = FALSE)
  mkdirp(dirname(tmp_destfile))

  http_get(url, file = tmp_destfile, ...)$
    then(http_stop_for_status)$
    then(function(resp) {
      "!DEBUG downloaded `url`"
      file.rename(tmp_destfile, destfile)
      etag <- parse_headers_list(resp$headers)[["etag"]]
      if (!is.null(etag_file) && !is.null(etag)) {
        mkdirp(dirname(etag_file))
        writeLines(etag, etag_file)
      }
      list(url = url, destfile = destfile, response = resp, etag = etag,
           etag_file = etag_file)
    })$
    catch(error = function(err) {
      "!DEBUG downloading `url` failed"
      err$destfile <- destfile
      err$url <- url
      stop(err)
    })
}

read_etag <- function(etag_file) {
  tryCatch(
    suppressWarnings(read_lines(etag_file, n = 1, warn = FALSE)[1]),
    error = function(e) NA
  )
}

get_etag_header_from_file <- function(destfile, etag_file) {
  if (!is.null(etag_file)) {
    etag_old <- read_etag(etag_file)
    if (file.exists(destfile) && !is.na(etag_old)) {
      c("If-None-Match" = etag_old)
    }
  }
}

#' Download a file, if it is newer than a local file
#'
#' A version of [download_file()] that only downloads if the file at the
#' specified URL is different from the local one.
#'
#' @inheritParams download_file
#' @param etag_file If not `NULL` then the path to a file that may contain
#'   the Etag of a previous request to this URL. If `destfile` exists, and
#'   `etag_file` exists and it is not empty, then the `If-None-Match` HTTP
#'   header is used with this Etag to avoid downloading the file if it has
#'   not changed. If the file at `url` has changed, then it is downloaded,
#'   and the the new Etag is stored in `etag_file`.
#' @inherit download_file return
#'
#' @family async HTTP tools
#' @keywords internal
#' @section Examples:
#' ```
#' dest <- tempfile(fileext = ".jpeg")
#' etag <- tempfile()
#' dl <- function() {
#'   ## This URL will repond with an Etag
#'   download_if_newer("https://httpbin.org/etag/test", dest,
#'                     etag_file = etag)
#' }
#' file.exists(dest)
#' file.exists(etag)
#'
#' res1 <- synchronise(dl())
#'
#' ## Downloaded the file, and also created the etag file
#' file.exists(dest)
#' file.exists(etag)
#' readLines(etag)
#' res1$response$status_code
#'
#' ## This will not download the file again, as the Etag matches
#' ## The status code is 304 Not Modified
#' res2 <- synchronise(dl())
#' res2$response$status_code
#'
#' ## HTTP errors contain the response
#' dest <- tempfile()
#' etag <- tempfile()
#' err <- tryCatch(
#'   synchronise(download_if_newer("https://httpbin.org/status/418",
#'                                 dest, etag)),
#'   error = function(e) e
#' )
#' err
#' names(err)
#' cat(rawToChar(err$response$content))
#' ```

download_if_newer <- function(url, destfile, etag_file = NULL,
                              headers = character(),
                              tmp_destfile = paste0(destfile, ".tmp"),
                              ...) {
  "!DEBUG download if newer `url`"
  assert_that(
    is_string(url),
    is_path(destfile),
    is_path(tmp_destfile),
    is_path_or_null(etag_file),
    is.character(headers), all_named(headers))
  force(list(...))

  etag_old <- get_etag_header_from_file(destfile, etag_file)
  headers <- c(headers, etag_old)

  destfile <- normalizePath(destfile, mustWork = FALSE)
  tmp_destfile <- normalizePath(tmp_destfile, mustWork = FALSE)
  mkdirp(dirname(tmp_destfile))

  http_get(url, file = tmp_destfile, headers = headers, ...)$
    then(http_stop_for_status)$
    then(function(resp) {
      if (resp$status_code == 304) {
        "!DEBUG download not needed, `url` current"
        etag <- unname(etag_old)
      } else if (resp$status_code == 200) {
        "!DEBUG downloaded `url`"
        file.rename(tmp_destfile, destfile)
        etag <- parse_headers_list(resp$headers)[["etag"]]
        if (!is.null(etag_file) && !is.null(etag)) {
          mkdirp(dirname(etag_file))
          writeLines(etag, etag_file)
        }
      } else {
        err <- structure(
          list(response = resp, message = "Unknown HTTP response"),
          class = "error")
        stop(err)
      }
      list(url = url, destfile = destfile, response = resp, etag = etag,
           etag_file = etag_file)
    })$
    catch(error = function(err) {
      "!DEBUG downloading `url` failed"
      err$destfile <- destfile
      err$url <- url
      stop(err)
    })

}

#' Download a files from multiple candidate URLs
#'
#' Uses [download_if_newer()] to starts downloads in parallel, and the
#' download that completes first is kept. (The others will be cancelled.)
#' Download errors are ignored, as long as at least one download completes
#' successfully.
#'
#' It also uses Etags, so if the destination file already exists, and one
#' of the URLs contain the same file (and this request completes first),
#' the file is not downloaded again.
#'
#' If all URLs fail, then `download_one_of` throws an error of class
#' `download_one_of_error`. The error object contains all errors from
#' the underlying [download_if_newer()] calls, in a list, in the
#' `errors` member.
#'
#' @inheritParams download_if_newer
#' @param urls A non-empty character vector of alternative URLs to try.
#' @inherit download_if_newer return
#'
#' @family async HTTP tools
#' @keywords internal
#' @section Examples:
#' ```
#' dest <- tempfile()
#' ## The first URL answers after a 1s delay,
#' ## the second after a 10s delay,
#' ## the third throws an error immediately, so it will be ignored.
#' ## Once the first URL responds, the second is cancelled, so the call
#' ## will finish well before the 10s are over.
#' dl <- function() {
#'   download_one_of(
#'     c("https://httpbin.org/delay/1",
#'       "https://httpbin.org/delay/10",
#'       "https://httpbin.org/status/404"),
#'     dest)
#' }
#' system.time(res <- synchronise(dl()))
#' file.exists(dest)
#' readLines(dest)
#'
#' ## Which URL responded
#' res$response$url
#'
#' ## If all URLs fail
#' dl2 <- function() {
#'   download_one_of(
#'     c("https://httpbin.org/status/418",
#'       "https://httpbin.org/status/401"),
#'     tempfile()
#'   )
#' }
#' res <- tryCatch(synchronise(dl2()), error = function(e) e)
#' res
#' res$errors
#' cat(rawToChar(res$errors[[1]]$response$content))
#' ```

download_one_of <- function(urls, destfile, etag_file = NULL,
                            headers = character(), ...) {
  "!DEBUG trying multiple URLs"
  assert_that(
    is_character(urls),  length(urls) >= 1,
    is_path(destfile),
    is_path_or_null(etag_file),
    is.character(headers), all_named(headers))
  force(list(...))

  tmps <- paste0(destfile, ".tmp.", seq_along(urls))
  dls <- mapply(
    download_if_newer, url = urls, tmp_destfile = tmps,
    MoreArgs = list(destfile = destfile, etag_file = etag_file,
                    headers = headers, ...),
    SIMPLIFY = FALSE)

  when_any(.list = dls)$
    catch(error = function(err) {
      err$message <- "All URLs failed"
      class(err) <- c("download_one_of_error", class(err))
      stop(err)
    })
}
