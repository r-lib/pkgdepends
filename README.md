
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgdepends

> Package Dependency Resolution, Downloads and Installation

<!-- badges: start -->

[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://github.com/r-lib/pkgdepends/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/pkgdepends/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/r-lib/pkgdepends/master.svg)](https://codecov.io/github/r-lib/pkgdepends?branch=master)
<!-- badges: end -->

pkgdepends is a toolkit for package dependencies, downloads and
installations, to be used in other packages. If you are looking for a
package manager, see [pak](https://github.com/r-lib/pak).

# Features

  - Look up package dependencies recursively.
  - Visualize package dependencies.
  - Download packages and their dependencies.
  - Install downloaded packages.
  - Includes a dependency solver to find a consistent set of
    dependencies.
  - Supports CRAN and Bioconductor packages automatically.
  - Supports packages on GitHub.
  - Supports local package file and trees.
  - Supports the `Remotes` entry in the `DESCRIPTION` file.
  - Caches metadata and downloaded packages via
    [pkgcache](https://github.com/r-lib/pkgcache)
  - Performs all downloads and HTTP queries concurrently.
  - Builds and installs packages in parallel.

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

See [“Package references”](TODO) for details.

## Package dependencies

Dependencies of the development version of the cli package:

``` r
pd <- new_pkg_deps("r-lib/pkgcache")
pd$solve()
pd$draw()
```

    #> r-lib/pkgcache 1.1.1.9000 [new][bld][cmp][dl] (unknown size)
    #> +-assertthat 0.2.1 [new][dl] (52.47 kB)
    #> +-callr 3.5.1 [new]
    #> | +-processx 3.4.5 [new][dl] (unknown size)
    #> | | +-ps 1.4.0 [new]
    #> | | \-R6 2.5.0 [new]
    #> | \-R6
    #> +-cli 2.2.0 [new]
    #> | +-assertthat
    #> | +-crayon 1.3.4 [new][dl] (748.25 kB)
    #> | +-glue 1.4.2 [new]
    #> | \-fansi 0.4.1 [new][dl] (210.40 kB)
    #> +-curl 4.3 [new][dl] (741.06 kB)
    #> +-digest 0.6.27 [new]
    #> +-filelock 1.0.2 [new][dl] (26.67 kB)
    #> +-glue
    #> +-prettyunits 1.1.1 [new][dl] (34.79 kB)
    #> +-R6
    #> +-processx
    #> +-rappdirs 0.3.1 [new][dl] (145.56 kB)
    #> +-rlang 0.4.9 [new]
    #> +-tibble 3.0.4 [new]
    #> | +-cli
    #> | +-crayon
    #> | +-ellipsis 0.3.1 [new][dl] (33.48 kB)
    #> | | \-rlang
    #> | +-fansi
    #> | +-lifecycle 0.2.0 [new][dl] (91.64 kB)
    #> | | +-glue
    #> | | \-rlang
    #> | +-magrittr 2.0.1 [new]
    #> | +-pillar 1.4.7 [new]
    #> | | +-cli
    #> | | +-crayon
    #> | | +-ellipsis
    #> | | +-fansi
    #> | | +-lifecycle
    #> | | +-rlang
    #> | | +-utf8 1.1.4 [new][dl] (195.28 kB)
    #> | | \-vctrs 0.3.5 [new]
    #> | |   +-ellipsis
    #> | |   +-digest
    #> | |   +-glue
    #> | |   \-rlang
    #> | +-pkgconfig 2.0.3 [new][dl] (17.63 kB)
    #> | +-rlang
    #> | \-vctrs
    #> \-uuid 0.1-4 [new][dl] (27.75 kB)
    #> 
    #> Key:  [new] new | [dl] download | [bld] build | [cmp] compile

See the [`pkg_deps`](TODO) class for details.

## Package downloads

Downloading all dependencies of a package:

``` r
pdl <- new_pkg_download_proposal("r-lib/cli")
pdl$resolve()
pdl$download()
```

See the [`pkg_download_proposal`](TODO) class for details.

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

[`pkg_deps`](TODO), [`pkg_download_proposal`](TODO) and
[`pkg_installation_proposal`](TODO) all resolve their dependencies
recursively, to obtain information about all packages needed for the
specified [package references](TODO). See [“Dependency
resolution”](TODO) for details.

## The dependency solver

The dependency solver takes the resolution information, and works out
the exact versions of each package that must be installed, such that
version and other requirements are satisfied. See [“The dependency
solver”](TODO) for details.

## Installation plans

[`pkg_installation_proposal`](TODO) can create installation plans, and
then also install them. It is also possible to import installation plans
that were created by other tools. See [“Installation plans”](TODO) for
details.

## Configuration

The details of [`pkg_deps`](TODO), [`pkg_download_proposal`](TODO) and
[`pkg_installation_proposal`](TODO) can be tuned with a list of
configuration options. See [“Configuration”](TODO) for details.

# Related

  - [pak](https://github.com/r-lib/pak) – R package manager
  - [pkgcache](https://github.com/r-lib/pkgcache) – Metadata and package
    cache
  - [devtools](https://github.com/r-lib/devtools) – Tools for R package
    developers

# License

MIT (c) RStudio
