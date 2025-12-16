# R6 class for package downloads

Download packages with their dependencies, from various sources.

## Usage

``` r
new_pkg_download_proposal(refs, ...)
```

## Arguments

- refs:

  Package names or references. See ['Package
  references'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for the syntax.

- ...:

  Additional arguments, passed to
  [`pkg_download_proposal$new()`](#method-new).

## Value

`new_pkg_download_proposal()` returns a new `pkg_download_proposal`
object.

## Details

`new_pkg_download_proposal()` creates a new object from the
`pkg_download_proposal` class, that can be used to look up and download
R packages and their dependencies. The advantage of
`new_pkg_download_proposal()` compared to using the
pkg_download_proposal constructor directly is that it avoids making
pkgdepends a build time dependency.

Typical workflow to download a set of packages:

1.  Create a `pkg_download_proposal` object with
    `new_pkg_download_proposal()`.

2.  Resolve all possible dependencies with
    [`pkg_download_proposal$resolve()`](#method-resolve).

3.  Download all files with
    [`pkg_download_proposal$download()`](#method-download).

4.  Get the data about the packages and downloads with
    [`pkg_download_proposal$get_downloads()`](#method-get-downloads).

## Methods

### Public methods

- [`pkg_download_proposal$new()`](#method-pkg_download_proposal-new)

- [`pkg_download_proposal$get_refs()`](#method-pkg_download_proposal-get_refs)

- [`pkg_download_proposal$get_config()`](#method-pkg_download_proposal-get_config)

- [`pkg_download_proposal$resolve()`](#method-pkg_download_proposal-resolve)

- [`pkg_download_proposal$async_resolve()`](#method-pkg_download_proposal-async_resolve)

- [`pkg_download_proposal$get_resolution()`](#method-pkg_download_proposal-get_resolution)

- [`pkg_download_proposal$download()`](#method-pkg_download_proposal-download)

- [`pkg_download_proposal$async_download()`](#method-pkg_download_proposal-async_download)

- [`pkg_download_proposal$get_downloads()`](#method-pkg_download_proposal-get_downloads)

- [`pkg_download_proposal$stop_for_download_error()`](#method-pkg_download_proposal-stop_for_download_error)

- [`pkg_download_proposal$format()`](#method-pkg_download_proposal-format)

- [`pkg_download_proposal$print()`](#method-pkg_download_proposal-print)

- [`pkg_download_proposal$clone()`](#method-pkg_download_proposal-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `pkg_download_proposal` object. Consider using
`new_pkg_download_proposal()` instead of calling the constructor
directly.

The returned object can be used to look up (recursive) dependencies of R
packages from various sources, and then to download the package files.

#### Usage

    pkg_download_proposal$new(refs, config = list(), remote_types = NULL)

#### Arguments

- `refs`:

  Package names or references. See ['Package
  references'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for the syntax.

- `config`:

  Configuration options, a named list. See
  ['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).

- `remote_types`:

  Custom remote ref types, this is for advanced use, and experimental
  currently.

  ##### Examples

      pdl <- pkg_download_proposal$new("r-lib/pkgdepends")
      pdl

------------------------------------------------------------------------

### Method `get_refs()`

The package refs that were used to create the `pkg_download_proposal`
object.

#### Usage

    pkg_download_proposal$get_refs()

#### Returns

A character vector of package refs that were used to create the
`pkg_download_proposal` object.

------------------------------------------------------------------------

### Method `get_config()`

Configuration options for the `pkg_download_proposal` object. See
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for details.

#### Usage

    pkg_download_proposal$get_config()

#### Returns

Named list. See
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for the configuration options.

------------------------------------------------------------------------

### Method `resolve()`

Resolve the dependencies of the specified package references. This
usually means downloading metadata from CRAN and Bioconductor, unless
already cached, and also from GitHub if GitHub refs were included,
either directly or indirectly. See ['Dependency
resolution'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for details.

#### Usage

    pkg_download_proposal$resolve()

#### Returns

The `pkg_download_proposal` object itself, invisibly.

------------------------------------------------------------------------

### Method `async_resolve()`

The same as [`resolve()`](#method-resolve), but asynchronous. This
method is for advanced use.

#### Usage

    pkg_download_proposal$async_resolve()

#### Returns

A deferred value.

------------------------------------------------------------------------

### Method `get_resolution()`

Query the result of the dependency resolution. This method can be called
after [`resolve()`](#method-resolve) has completed.

#### Usage

    pkg_download_proposal$get_resolution()

#### Returns

A
[pkg_resolution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
object, which is also a data frame. See ['Dependency
resolution'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for its columns.

------------------------------------------------------------------------

### Method `download()`

Download all resolved packages. It uses the package cache in the
pkgcache package by default, to avoid downloads if possible.

#### Usage

    pkg_download_proposal$download()

#### Returns

The `pkg_download_proposal` object, invisibly.

------------------------------------------------------------------------

### Method `async_download()`

The same as [`download()`](#method-download), but asynchronous. This
method is for advanced use.

#### Usage

    pkg_download_proposal$async_download()

#### Returns

A deferred value.

------------------------------------------------------------------------

### Method `get_downloads()`

Returns the summary of the package downloads.

#### Usage

    pkg_download_proposal$get_downloads()

#### Returns

A
[pkg_download_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
object, which is a list. See
[pkg_download_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
for details.

------------------------------------------------------------------------

### Method `stop_for_download_error()`

Throw and error if the some of the downloads have failed for the most
recent [`pkg_download_proposal$download()`](#method-download) call.

#### Usage

    pkg_download_proposal$stop_for_download_error()

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Format a `pkg_download_proposal` object, typically for printing.

#### Usage

    pkg_download_proposal$format(...)

#### Arguments

- `...`:

  not used currently.

#### Returns

Nothing. A character vector, each element should be a line in the
printout.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a `pkg_download_proposal` object to the screen. The printout
includes:

- The package refs.

- Whether the object has the resolved dependencies.

- Whether the resolution had errors.

- Whether the downloads were completed.

- Whether the downloads had errors.

- Advice on which methods to call next.

See the example below.

#### Usage

    pkg_download_proposal$print(...)

#### Arguments

- `...`:

  not used currently.

#### Returns

The `pkg_download_proposal` object itself, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    pkg_download_proposal$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Method get_refs()
pdl <- new_pkg_download_proposal(c("pak", "jsonlite"))
pdl$get_refs()
#> [1] "pak"      "jsonlite"
# Method get_config()
pdl <- new_pkg_download_proposal("pak")
pdl$get_config()
#> # pkg config
#> ## build_vignettes
#> <default>
#> [1] FALSE
#> 
#> ## sysreqs_verbose
#> <default>
#> [1] TRUE
#> 
#> ## sysreqs_db_update
#> <default>
#> [1] TRUE
#> 
#> ## metadata_cache_dir
#> <default>
#> [1] "/tmp/RtmpIBIZjx/file1dd92d4d633d"
#> 
#> ## platforms
#> <default>
#> [1] "x86_64-pc-linux-gnu-ubuntu-24.04"
#> [2] "source"                          
#> 
#> ## goal
#> <default>
#> [1] "unknown"
#> 
#> ## r_versions
#> <default>
#> [1] "4.5.2"
#> 
#> ## cache_dir
#> <default>
#> [1] "/tmp/RtmpIBIZjx/file1dd9148ab544"
#> 
#> ## library
#> <default>
#> NULL
#> 
#> ## metadata_update_after
#> <default>
#> Time difference of 24 hours
#> 
#> ## include_linkingto
#> <default>
#> [1] FALSE
#> 
#> ## sysreqs_rspm_repo_id
#> <default>
#> [1] "1"
#> 
#> ## sysreqs_update
#> <default>
#> [1] TRUE
#> 
#> ## package_cache_dir
#> <default>
#> NULL
#> 
#> ## sysreqs_rspm_url
#> <default>
#> [1] "https://packagemanager.posit.co"
#> 
#> ## sysreqs_sudo
#> <default>
#> [1] TRUE
#> 
#> ## sysreqs_db_update_timeout
#> <default>
#> Time difference of 60 secs
#> 
#> ## sysreqs_lookup_system
#> <default>
#> [1] TRUE
#> 
#> ## git_submodules
#> <default>
#> [1] FALSE
#> 
#> ## sysreqs_platform
#> <default>
#> [1] "x86_64-pc-linux-gnu-ubuntu-24.04"
#> 
#> ## dependencies
#> <default>
#> [1] "Depends"   "Imports"   "LinkingTo"
#> 
#> ## sysreqs
#> <default>
#> [1] TRUE
#> 
#> ## sysreqs_dry_run
#> <default>
#> [1] FALSE
#> 
#> ## windows_archs
#> <default>
#> [1] "prefer-x64"
#> 
#> ## use_bioconductor
#> <default>
#> [1] TRUE
#> 
#> ## cran_mirror
#> <default>
#>                       CRAN 
#> "https://cran.rstudio.com" 
#> 
# Method resolve()
pdl <- new_pkg_download_proposal("pak")
pdl$resolve()
pdl$get_resolution()
#> # A data frame: 2 × 35
#>   ref   type     direct directpkg status package version license
#>   <chr> <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
#> 1 pak   standard TRUE   TRUE      OK     pak     0.9.1   GPL-3  
#> 2 pak   standard TRUE   TRUE      OK     pak     0.9.1   GPL-3  
#> # ℹ 27 more variables: needscompilation <lgl>, priority <chr>,
#> #   md5sum <chr>, sha256 <chr>, filesize <int>, built <chr>,
#> #   platform <chr>, rversion <chr>, repotype <chr>, repodir <chr>,
#> #   target <chr>, deps <list>, mirror <chr>, sources <list>,
#> #   remote <list>, error <list>, metadata <list>, extra <list>,
#> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>,
#> #   cache_status <chr>, sysreqs_packages <list>, …
# Method get_resolution()
pdl <- new_pkg_download_proposal("r-lib/pkgdepends")
pdl$resolve()
pdl$get_resolution()
#> # A data frame: 27 × 35
#>    ref            type  direct directpkg status package version license
#>    <chr>          <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
#>  1 r-lib/pkgdepe… gith… TRUE   TRUE      OK     pkgdep… 0.9.0.… MIT + …
#>  2 callr          stan… FALSE  FALSE     OK     callr   3.7.6   MIT + …
#>  3 cli            stan… FALSE  FALSE     OK     cli     3.6.5   MIT + …
#>  4 curl           stan… FALSE  FALSE     OK     curl    7.0.0   MIT + …
#>  5 desc           stan… FALSE  FALSE     OK     desc    1.4.3   MIT + …
#>  6 filelock       stan… FALSE  FALSE     OK     filelo… 1.0.3   MIT + …
#>  7 jsonlite       stan… FALSE  FALSE     OK     jsonli… 2.0.0   MIT + …
#>  8 lpSolve        stan… FALSE  FALSE     OK     lpSolve 5.6.23  LGPL-2 
#>  9 pkgbuild       stan… FALSE  FALSE     OK     pkgbui… 1.4.8   MIT + …
#> 10 pkgcache       stan… FALSE  FALSE     OK     pkgcac… 2.2.4   MIT + …
#> # ℹ 17 more rows
#> # ℹ 27 more variables: needscompilation <lgl>, priority <chr>,
#> #   md5sum <chr>, sha256 <chr>, filesize <int>, built <chr>,
#> #   platform <chr>, rversion <chr>, repotype <chr>, repodir <chr>,
#> #   target <chr>, deps <list>, mirror <chr>, sources <list>,
#> #   remote <list>, error <list>, metadata <list>, extra <list>,
#> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>, …
# Method download()
pdl <- new_pkg_download_proposal("r-lib/pkgdepends")
pdl$resolve()
pdl$download()
#> ℹ Getting 13 pkgs (3.95 MB) and 14 pkgs with unknown sizes
#> ✔ Got callr 3.7.6 (source) (104.36 kB)
#> ✔ Got filelock 1.0.3 (source) (15.44 kB)
#> ✔ Got jsonlite 2.0.0 (source) (1.06 MB)
#> ✔ Got curl 7.0.0 (source) (731.11 kB)
#> ✔ Got cli 3.6.5 (source) (640.24 kB)
#> ✔ Got desc 1.4.3 (source) (80.07 kB)
#> ✔ Got pkgcache 2.2.4 (source) (292.44 kB)
#> ✔ Got pkgbuild 1.4.8 (source) (51.30 kB)
#> ✔ Got processx 3.8.6 (source) (165.19 kB)
#> ✔ Got ps 1.9.1 (source) (167.92 kB)
#> ✔ Got R6 2.6.1 (source) (64.51 kB)
#> ✔ Got lpSolve 5.6.23 (source) (467.59 kB)
#> ✔ Got zip 2.3.3 (source) (115.47 kB)
#> ✔ Got filelock 1.0.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (24.70 kB)
#> ✔ Got desc 1.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (334.52 kB)
#> ✔ Got jsonlite 2.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.09 MB)
#> ✔ Got R6 2.6.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (86.81 kB)
#> ✔ Got callr 3.7.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (449.24 kB)
#> ✔ Got curl 7.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (788.30 kB)
#> ✔ Got pkgbuild 1.4.8 (x86_64-pc-linux-gnu-ubuntu-24.04) (208.62 kB)
#> ✔ Got processx 3.8.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (337.39 kB)
#> ✔ Got cli 3.6.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.34 MB)
#> ✔ Got ps 1.9.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (499.64 kB)
#> ✔ Got zip 2.3.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (660.96 kB)
#> ✔ Got pkgdepends 0.9.0.9000 (source) (1.72 MB)
#> ✔ Got pkgcache 2.2.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (961.77 kB)
#> ✔ Got lpSolve 5.6.23 (x86_64-pc-linux-gnu-ubuntu-24.04) (374.14 kB)
pdl$get_downloads()
#> # A data frame: 27 × 41
#>    ref            type  direct directpkg status package version license
#>    <chr>          <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
#>  1 r-lib/pkgdepe… gith… TRUE   TRUE      OK     pkgdep… 0.9.0.… MIT + …
#>  2 callr          stan… FALSE  FALSE     OK     callr   3.7.6   MIT + …
#>  3 cli            stan… FALSE  FALSE     OK     cli     3.6.5   MIT + …
#>  4 curl           stan… FALSE  FALSE     OK     curl    7.0.0   MIT + …
#>  5 desc           stan… FALSE  FALSE     OK     desc    1.4.3   MIT + …
#>  6 filelock       stan… FALSE  FALSE     OK     filelo… 1.0.3   MIT + …
#>  7 jsonlite       stan… FALSE  FALSE     OK     jsonli… 2.0.0   MIT + …
#>  8 lpSolve        stan… FALSE  FALSE     OK     lpSolve 5.6.23  LGPL-2 
#>  9 pkgbuild       stan… FALSE  FALSE     OK     pkgbui… 1.4.8   MIT + …
#> 10 pkgcache       stan… FALSE  FALSE     OK     pkgcac… 2.2.4   MIT + …
#> # ℹ 17 more rows
#> # ℹ 33 more variables: needscompilation <lgl>, priority <chr>,
#> #   md5sum <chr>, sha256 <chr>, filesize <int>, built <chr>,
#> #   platform <chr>, rversion <chr>, repotype <chr>, repodir <chr>,
#> #   target <chr>, deps <list>, mirror <chr>, sources <list>,
#> #   remote <list>, error <list>, metadata <list>, extra <list>,
#> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>, …
# Method get_downloads()
pdl <- new_pkg_download_proposal("pkgload")
pdl$resolve()
pdl$download()
#> ℹ Getting 6 pkgs (2.35 MB) and 13 pkgs with unknown sizes, 7 (1.27 MB) cached
#> ✔ Got glue 1.8.0 (source) (126.68 kB)
#> ✔ Got lifecycle 1.0.4 (source) (107.66 kB)
#> ✔ Got fs 1.6.6 (source) (1.20 MB)
#> ✔ Got pkgload 1.4.1 (source) (87.51 kB)
#> ✔ Got rprojroot 2.1.1 (source) (59.90 kB)
#> ✔ Got rlang 1.1.6 (source) (767.93 kB)
#> ✔ Cached copy of callr 3.7.6 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of cli 3.6.5 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of desc 1.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of processx 3.8.6 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of ps 1.9.1 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of R6 2.6.1 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Got fs 1.6.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (310.07 kB)
#> ✔ Got lifecycle 1.0.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (125.07 kB)
#> ✔ Cached copy of pkgbuild 1.4.8 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Got glue 1.8.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (168.12 kB)
#> ✔ Got rprojroot 2.1.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (113.23 kB)
#> ✔ Got pkgload 1.4.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (222.06 kB)
#> ✔ Got rlang 1.1.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.59 MB)
pdl$get_downloads()
#> # A data frame: 26 × 41
#>    ref       type     direct directpkg status package   version license
#>    <chr>     <chr>    <lgl>  <lgl>     <chr>  <chr>     <chr>   <chr>  
#>  1 callr     standard FALSE  FALSE     OK     callr     3.7.6   MIT + …
#>  2 cli       standard FALSE  FALSE     OK     cli       3.6.5   MIT + …
#>  3 desc      standard FALSE  FALSE     OK     desc      1.4.3   MIT + …
#>  4 fs        standard FALSE  FALSE     OK     fs        1.6.6   MIT + …
#>  5 glue      standard FALSE  FALSE     OK     glue      1.8.0   MIT + …
#>  6 lifecycle standard FALSE  FALSE     OK     lifecycle 1.0.4   MIT + …
#>  7 pkgbuild  standard FALSE  FALSE     OK     pkgbuild  1.4.8   MIT + …
#>  8 pkgload   standard TRUE   TRUE      OK     pkgload   1.4.1   MIT + …
#>  9 processx  standard FALSE  FALSE     OK     processx  3.8.6   MIT + …
#> 10 ps        standard FALSE  FALSE     OK     ps        1.9.1   MIT + …
#> # ℹ 16 more rows
#> # ℹ 33 more variables: needscompilation <lgl>, priority <chr>,
#> #   md5sum <chr>, sha256 <chr>, filesize <int>, built <chr>,
#> #   platform <chr>, rversion <chr>, repotype <chr>, repodir <chr>,
#> #   target <chr>, deps <list>, mirror <chr>, sources <list>,
#> #   remote <list>, error <list>, metadata <list>, extra <list>,
#> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>, …
# Method print()
pdl <- new_pkg_download_proposal("r-lib/pkgdepends")
pdl
#> <pkg_download_proposal>
#> + refs:
#>   - r-lib/pkgdepends
#> (use `$resolve()` to resolve dependencies)

pdl$resolve()
pdl
#> <pkg_download_proposal>
#> + refs:
#>   - r-lib/pkgdepends
#> + has resolution (+13 dependencies)
#> (use `$download()` to download packages)
#> (use `$get_resolution()` to see resolution results)

pdl$download()
#> ℹ Getting 14 pkgs with unknown sizes, 13 (3.95 MB) cached
#> ✔ Cached copy of pkgdepends 0.9.0.9000 (source) is the latest build
#> ✔ Cached copy of callr 3.7.6 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of cli 3.6.5 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of curl 7.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of desc 1.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of filelock 1.0.3 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of jsonlite 2.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of ps 1.9.1 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of zip 2.3.3 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of pkgbuild 1.4.8 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of lpSolve 5.6.23 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of processx 3.8.6 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of R6 2.6.1 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
#> ✔ Cached copy of pkgcache 2.2.4 (x86_64-pc-linux-gnu-ubuntu-24.04) is the latest build
pdl
#> <pkg_download_proposal>
#> + refs:
#>   - r-lib/pkgdepends
#> + has resolution (+13 dependencies)
#> + has downloads
#> (use `$get_resolution()` to see resolution results)
#> (use `$get_downloads()` to get download data)
```
