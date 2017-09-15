
remotes__download_cran <- function(self, private, resolution) {

  ref <- resolution$remote$ref
  message("Downloading ", ref)

  repo <- private$repo

  async_map(resolution$files, function(files) {
    urls <- files$source
    target_file <- file.path(
      private$get_download_cache_dir(),
      files$target
    )
    mkdirp(dirname(target_file))
    etag_file <- file.path(target_dir, "_cache", basename(target_file))

    had_this <- first_existing_file(target_file, target_file)
    download_try_list(urls, target_file, etag_file)$
      then(function(status) {
        if (status == 304) {
          make_dl_status("Had", files, urls, target_file,
                         bytes = file.size(target_file))
        } else {
          make_dl_status("Got", files, urls, target_file,
                         bytes = file.size(target_file))
        }
      })$
      catch(function(err) {
        make_dl_status("Failed", files, urls, target_file,
                       error = err$error)
      })
  })
}
