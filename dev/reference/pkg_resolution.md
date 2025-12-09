# Dependency resolution

Collect information about dependencies of R packages, recursively.

## Details

[`pkg_deps`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
all resolve their dependencies recursively, to obtain information about
all packages needed for the specified [package
references](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md).

### CRAN and Bioconductor packages

Resolution currently start by downloading the CRAN and Bioconductor
metadata, if it is out of date. For CRAN, we also download additional
metadata, that includes file sizes, SHA hashes, system requirements, and
"built" (for binary packages) and "packaged" time stamps. The extra meta
information is updated daily currently, so for some packages it might be
incorrect or missing.

### GitHub packages

For GitHub packages, we query their download URL to be able to download
the package later, and also download their `DESCRIPTION` file, to learn
about their dependencies.

### Local packages

From local package files we extract the `DESCRIPTION` file, to learn
about their dependencies.

### The `remotes` field in `DESCRIPTION`

We support the non-standard `Remotes` field in the package `DESCRIPTION`
file. This field may contain a list of package references for any of the
dependencies that are specified in one of the `Depends`, `Includes`,
`Suggests` or `Enhances` fields. The syntax is a comma separated list of
[package
references](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md).

### The result

The result of the resolution is a data frame with information about the
packages and their dependencies.

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

Additional columns might be present. They are either used internally or
they are experimental. They might be removed or changed at any time.

All columns are of type character, except for `direct` (logical),
`needscompilation` (logical), `filesize` (integer), `deps` (list column,
see "Package dependency tables" below), `sources` (list of character
vectors), `remote` (list), `error` (list), `metadata` (list),
`dep_types` (list).

#### Package dependency tables

A package dependency tables in the `deps` list column have five columns
currently:

- `ref`: the package ref of the dependency.

- `type`: the dependency type, in all lowercase. I.e. `imports`,
  `suggests`, etc.

- `package`: package name of the dependency.

- `op`: operator for version requirements, e.g. `>=`.

- `version`: version number, for version requirements.

### Resolution failures

The resolution process does not stop on error. Instead, failed
resolutions return and error object in the `error` column of the result
data frame.
