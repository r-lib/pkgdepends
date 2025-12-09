# Package downloads

The
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
classes both have download methods, to downloads package files into a
configured directory (see
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)).

## Details

They return a `pkg_download_result` object, which is a data frame, that
adds extra columns to
[`pkg_resolution_result`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
(for
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md))
or
[`pkg_solution_result`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
(for
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)):

- `built`: the `Built` field from the `DESCRIPTION` file of binary
  packages, for which this information is available.

- `cache_status`: whether the package file is in the package cache. It
  is `NA` for `installed::` package refs.

- `dep_types`: character vector of dependency types that were considered
  for this package. (This is a list column.)

- `deps`: dependencies of the package, in a data frame. See "Package
  dependency tables" below.

- `direct`: whether this package (ref, really) was directly specified,
  or added as a dependency.

- `error`: this is a list column that contains error objects for the
  refs that pkgdepends failed to resolve.

- `filesize`: the file size in bytes, or `NA` if this information is not
  available.

- `license`: license of the package, or `NA` if not available.

- `md5sum`: MD5 checksum of the package file, if available, or `NA` if
  not.

- `metadata`: a named character vector. These fields will be (should be)
  added to the installed `DESCRIPTION` file of the package.

- `mirror`: URL of the CRAN(-like) mirror site where the metadata was
  obtained from. It is NA for non-CRAN-like sources, e.g. local files,
  installed packages, GitHub, etc.

- `needscompilation`: whether the package needs compilation.

- `package`: package name.

- `priority`: this is `"base"` for base packages, `"recommended"` for
  recommended packages, and `NA` otherwise.

- `ref`: package reference.

- `remote`: the parsed `remote_ref` objects, see
  [`parse_pkg_refs()`](https://r-lib.github.io/pkgdepends/dev/reference/parse_pkg_refs.md).
  This is a list column.

- `repodir`: the directory where this package should be in a CRAN-like
  repository.

- `sha256`: SHA256 hash of the package file, if available, otherwise
  `NA`.

- `sources`: URLs where this package can be downloaded from. This is not
  necessarily a URL that you can download with a HTTP client. E.g. for
  `local::` refs it is a path, and for `git::` refs it is a URL for git.
  It is a zero length vector for `installed::` refs.

- `status`: status of the dependency resolution, `"OK"` or `"FAILED"`.

- `target`: path where this package should be saved in a
  CRAN-repository.

- `type`: ref type.

- `version`: package version.

- `fulltarget`: absolute path to the downloaded file. At most one of
  `fulltarget` and `fulltarget_tree` must exist on the disk.

- `fulltarget_tree`: absolute path to a package tree directory. At most
  one of `fulltarget` and `fulltarget_tree` must exist on the disk.

- `download_status`: `"Had"` or `"Got"`, depending on whether the file
  was obtained from the cache.

- `download_error`: error object for failed downloads.

- `file_size`: Size of the file, or `NA`. For `installed::` refs, it is
  `NA`, and it is also `NA` for refs that created `fulltarget_tree`
  instead of `fulltarget`.

`fulltarget`, if it exists, contains a packaged (via `R CMD build`)
source R package. If `fulltarget_tree` exists, it is a package tree
directory, that still needs an `R CMD build` call.

Additional columns might be present. They are either used internally or
they are experimental. They might be removed or changed at any time.

All columns are of type character, except for `direct` (logical),
`needscompilation` (logical), `filesize` (integer), `deps` (list column,
see "Package dependency tables" below), `sources` (list of character
vectors), `remote` (list), `error` (list), `metadata` (list),
`dep_types` (list).

### Package dependency tables

A package dependency tables in the `deps` list column have five columns
currently:

- `ref`: the package ref of the dependency.

- `type`: the dependency type, in all lowercase. I.e. `imports`,
  `suggests`, etc.

- `package`: package name of the dependency.

- `op`: operator for version requirements, e.g. `>=`.

- `version`: version number, for version requirements.
