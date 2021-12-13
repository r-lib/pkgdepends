
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgdepends

> Package Dependency Resolution, Downloads and Installation

<!-- badges: start -->

[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://github.com/r-lib/pkgdepends/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/pkgdepends/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/r-lib/pkgdepends/main.svg)](https://codecov.io/github/r-lib/pkgdepends?branch=main)
<!-- badges: end -->

pkgdepends is a toolkit for package dependencies, downloads and
installations, to be used in other packages. If you are looking for a
package manager, see [pak](https://github.com/r-lib/pak).

# Features

-   Look up package dependencies recursively.
-   Visualize package dependencies.
-   Download packages and their dependencies.
-   Install downloaded packages.
-   Includes a dependency solver to find a consistent set of
    dependencies.
-   Supports CRAN and Bioconductor packages automatically.
-   Supports packages on GitHub.
-   Supports local package file and trees.
-   Supports the `Remotes` entry in the `DESCRIPTION` file.
-   Caches metadata and downloaded packages via
    [pkgcache](https://github.com/r-lib/pkgcache)
-   Performs all downloads and HTTP queries concurrently.
-   Builds and installs packages in parallel.

# Install

Once on CRAN, install the package with:

``` r
install.packages("pkgdepends")
```

# Usage

``` r
library(pkgdepends)
```

## Package references

A package reference (ref) specifies a location from which an R package
can be obtained from. Examples:

    devtools
    cran::devtools
    bioc::Biobase
    r-lib/pkgdepends
    https://github.com/r-lib/pkgdepends
    local::~/works/shiny

See [“Package
references”](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html)
for details.

## Package dependencies

Dependencies of the development version of the cli package:

``` r
pd <- new_pkg_deps("r-lib/pkgcache")
pd$solve()
pd$draw()
```

    #> r-lib/pkgcache 1.3.0.9000 [new][bld][cmp][dl] (unknown size)
    #> +-assertthat 0.2.1 [new][dl] (52.27 kB)
    #> +-callr 3.7.0 [new][dl] (437.95 kB)
    #> | +-processx 3.5.2 [new][dl] (299.05 kB)
    #> | | +-ps 1.6.0 [new][dl] (286.34 kB)
    #> | | \-R6 2.5.1 [new][dl] (82.58 kB)
    #> | \-R6
    #> +-cli 3.1.0 [new]
    #> | \-glue 1.5.0 [new]
    #> +-curl 4.3.2 [new][dl] (880.06 kB)
    #> +-digest 0.6.28 [new][dl] (292.25 kB)
    #> +-filelock 1.0.2 [new][dl] (29.14 kB)
    #> +-glue
    #> +-jsonlite 1.7.2 [new][dl] (509.74 kB)
    #> +-prettyunits 1.1.1 [new][dl] (34.73 kB)
    #> +-R6
    #> +-processx
    #> +-rappdirs 0.3.3 [new][dl] (46.69 kB)
    #> +-rlang 0.4.12 [new][dl] (1.36 MB)
    #> +-tibble 3.1.6 [new]
    #> | +-ellipsis 0.3.2 [new][dl] (38.58 kB)
    #> | | \-rlang
    #> | +-fansi 0.5.0 [new][dl] (251.70 kB)
    #> | +-lifecycle 1.0.1 [new]
    #> | | +-glue
    #> | | \-rlang
    #> | +-magrittr 2.0.1 [new][dl] (226.63 kB)
    #> | +-pillar 1.6.4 [new][dl] (1.03 MB)
    #> | | +-cli
    #> | | +-crayon 1.4.2 [new][dl] (155.29 kB)
    #> | | +-ellipsis
    #> | | +-fansi
    #> | | +-lifecycle
    #> | | +-rlang
    #> | | +-utf8 1.2.2 [new][dl] (210.68 kB)
    #> | | \-vctrs 0.3.8 [new][dl] (1.45 MB)
    #> | |   +-ellipsis
    #> | |   +-glue
    #> | |   \-rlang
    #> | +-pkgconfig 2.0.3 [new][dl] (17.81 kB)
    #> | +-rlang
    #> | \-vctrs
    #> \-uuid 1.0-3 [new][dl] (48.79 kB)
    #> 
    #> Key:  [new] new | [dl] download | [bld] build | [cmp] compile

See the
[`pkg_deps`](https://r-lib.github.io/pkgdepends/reference/pkg_deps.html)
class for details.

## Package downloads

Downloading all dependencies of a package:

``` r
pdl <- new_pkg_download_proposal("r-lib/cli")
pdl$resolve()
pdl$download()
```

See the
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
class for details.

## Package installation

Installing or updating a set of package:

``` r
lib <- tempfile()
pdi <- new_pkg_installation_proposal(
  "r-lib/cli",
  config = list(library = lib)
)
pdi$solve()
pdi$download()
pdi$install()
```

## Dependency resolution

[`pkg_deps`](https://r-lib.github.io/pkgdepends/reference/pkg_deps.html),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
all resolve their dependencies recursively, to obtain information about
all packages needed for the specified [package
references](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html).
See [“Dependency
resolution”](https://r-lib.github.io/pkgdepends/reference/pkg_resolution.html)
for details.

## The dependency solver

The dependency solver takes the resolution information, and works out
the exact versions of each package that must be installed, such that
version and other requirements are satisfied. See [“The dependency
solver”](https://r-lib.github.io/pkgdepends/reference/pkg_solution.html)
for details.

## Installation plans

[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
can create installation plans, and then also install them. It is also
possible to import installation plans that were created by other tools.
See [“Installation
plans”](https://r-lib.github.io/pkgdepends/reference/install_plans.html)
for details.

## Configuration

The details of
[`pkg_deps`](https://r-lib.github.io/pkgdepends/reference/pkg_deps.html),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
can be tuned with a list of configuration options. See
[“Configuration”](https://r-lib.github.io/pkgdepends/reference/pkg_config.html)
for details.

# Related

-   [pak](https://github.com/r-lib/pak) – R package manager
-   [pkgcache](https://github.com/r-lib/pkgcache) – Metadata and package
    cache
-   [devtools](https://github.com/r-lib/devtools) – Tools for R package
    developers

# License

MIT (c) RStudio
