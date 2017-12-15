
#' Get package from cache, or download it asynchronously
#'
#' Currently, even if we take it from the cache, we check the etag
#' of the url. TODO: This should change in the future, and we should trust
#' at least CRAN source packages.
#'
#' @param cache `package_cache` instance.
#' @param urls Character vector, list of candidate urls.
#' @param target_file Where to save the file.
#' @return Download status.
#'
#' @keywords internal
#' @importFrom async async_detect

get_package_from <- function(cache, urls, cache_dir, target,
                             progress_bar = NULL) {
  cache ; urls ; cache_dir; target
  target_file <- file.path(cache_dir, target)
  mkdirp(target_dir <- dirname(target_file))

  etag_file <- tempfile()
  for (url in urls) {
    hit <- cache$copy_to(target_file, url = url)
    if (nrow(hit) >= 1) {
      writeLines(hit$etag, etag_file)
      break
    }
  }

  download_try_list(urls, target_file, etag_file,
                    progress_bar = progress_bar)$
    then(function(status) {
      if (status == 304) {
        make_dl_status("Had", urls, target_file,
                       bytes = file.size(target_file))
      } else {
        etag <- read_etag(etag_file)
        cache$add(target_file, path = target, package = NA_character_,
                  url = urls[1], etag = etag, md5 = NA_character_)
        make_dl_status("Got", urls, target_file,
                       bytes = file.size(target_file))
      }
    })$
    catch(function(err) {
      make_dl_status("Failed", urls, target_file,  error = err)
    })
}

