# GitHub Releases

Functions to query and manipulate GitHub releases. These functions are
currently experimental.

## Details

### List releases

#### Description

`ghr$list()` lists the last 100 releases for a GitHub repository.
`ghr$async_list()` is the async version of `ghr$list()`.

#### Usage

    ghr$list(repo)
    ghr$async_list(repo)

#### Arguments

- `repo`: repository slug, e.g. `"cran/cli"`.

#### Value

Data frame with columns:

- `id`: release id,

- `name`: release name, usually the version number, possibly with a `v`
  prefix: `3.6.1` or `v3.6.1`, but can be different.

- `tag_name`: usually the same as `name`.

- `created_at`: `POSIXct` vector.

### Get information about a release

#### Description

`ghr$get()` downloads information about a release, including release
assets.

`ghr$async_get` is the async version of `ghr$get`.

#### Usage

    ghr$get(repo, tag)

#### Arguments

- `repo`: repository slug, e.g. `"cran/cli"`.

- `tag`: tag to get.

#### Value

Named list, see
<https://docs.github.com/en/rest/releases/releases#get-a-release> for
the entries.

### List assets of a release

#### Description

`ghr$list_assets()` lists the last 100 assets of a release.

`ghr$async_list_assets()` is the async version of `ghr$list_assets()`

#### Usage

    ghr$list_assets(repo, tag)

#### Arguments

- `repo`: repository slug, e.g. `"cran/cli"`.

- `tag`: tag to query.

#### Value

Data frame with columns:

- `id`: asset id,

- `name`: file name of the asset,

- `download_url`: download URL,

- `size`: size in bytes,

- `created_at`: `POSIXct` vector,

- `updated_at`: `POSXct` vector,

- `content_type`: content type of asset.

### Add a release asset

#### Description

`ghr$add_asset()` adds an asset to a GitHub release.

`ghr$async_add_asset()` is the async version of `ghr$add_asset()`.

#### Usage

    ghr%add_asset(repo, file, tag, name = basename(file))

#### Arguments

- `repo`: repository slug, e.g. `cran/cli`.

- `file`: path to file to upload as an asset.

- `tag`: tag name to add the asset to. It must exist on GitHub.

- `name`: file name of the asset in the release.

#### Details

If an asset with the same name already exists, then that will be deleted
first.

#### Value

Response from GitHub as a named list. See
<https://docs.github.com/en/rest/releases/assets#upload-a-release-asset>
for the structure.

### Delete a release asset

#### Description

`ghr$delete_asset()` deleted a release asset.

`ghr$async_delete_asset()` is an async version of `ghr$delete_asset()`.

#### Usage

    ghr$delete_asset(repo, tag, name)
    ghr$async_delete_asset(repo, tag, name)

#### Arguments

- `repo`: repository slug, e.g. `cran/cli`.

- `tag`: tag name to create a release for. It must exist on GitHub.

- `name`: name of the asset.

#### Value

A list with entries:

- `release`: a list with the data about the release, before the
  deletion. It has the same format as the return value of `ghr$get()`.

- `deleted`: `TRUE` if the asset was deleted. `FALSE` if the asset did
  not exist.

### Create a GitHub release

#### Description

`ghr$create()` creates a GitHub release from a tag.

`ghr$async_create()` is an async version of `ghr$create()`.

#### Usage

    ghr$create(
      repo,
      tag,
      description = "",
      draft = FALSE,
      prerelease = FALSE,
      generage_release_notes = FALSE
    )

#### Arguments

- `repo`: repository slug, e.g. `cran/cli`.

- `tag`: tag name to create a release for. It must exist on GitHub.

- `description`: release description.

- `draft`: whether to create a draft release.

- `prerelease`: whether to create a prerelease.

- `generate_release_notes`: whether to auto-generate release notes.

#### Value

Response from GitHub as a named list. See
<https://docs.github.com/en/rest/releases/releases#create-a-release> for
the structure.
