# Download a blob for a remote git repository

Download a blob for a remote git repository

## Usage

``` r
git_download_file(url, sha, output = sha)
```

## Arguments

- url:

  Repository URL, e.g. `https://github.com/r-lib/pak.git`. It might
  include authentication information, e.g. a GitHub token.

- sha:

  SHA hash of the blob.

- output:

  Path where the blob will be written. It's directory is created if it
  does not exist.

## Value

A list that corresponds to a git packfile entry, invisibly. It has
entries: `type` (always `"blob"`), `object` (raw object, the blob
itself), `size`, `packed_size`, `hash`.

## Examples

Download a `DESCRIPTION` file from GitHub:

    pak_repo <- "https://github.com/r-lib/pak.git"
    fls <- git_list_files(pak_repo, "HEAD")
    git_download_file(
      pak_repo,
      fls$files$hash[fls$files$path == "DESCRIPTION"],
      output = tmp <- tempfile()
    )
    readLines(tmp)[1:5]
    #> [1] "Package: pak"
    #> [2] "Version: 0.3.1.9999"
    #> [3] "Title: Another Approach to Package Installation"
    #> [4] "Description: The goal of 'pak' is to make package installation faster and"
    #> [5] "    more reliable. In particular, it performs all HTTP operations in parallel,"
