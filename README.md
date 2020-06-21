
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgdepends

> Package Dependency Resolution, Downloads and
Installation

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![Linux Build
Status](https://travis-ci.org/r-lib/pkgdepends.svg?branch=master)](https://travis-ci.org/r-lib/pkgdepends)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/github/r-lib/pkgdepends?svg=true)](https://ci.appveyor.com/project/gaborcsardi/pkgdepends)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/pkgdepends)](http://www.r-pkg.org/pkg/pkgdepends)
[![Coverage
Status](https://img.shields.io/codecov/c/github/r-lib/pkgdepends/master.svg)](https://codecov.io/github/r-lib/pkgdepends?branch=master)

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
pd <- new_pkg_deps("r-lib/cli")
pd$solve()
pd$draw()
```

    #> cli (1.9.9.9000)
    #> ├─assertthat (0.2.1)
    #> ├─crayon (1.3.4)
    #> ├─glue (1.3.1)
    #> ├─progress (1.2.2)
    #> │ ├─hms (0.5.1)
    #> │ │ ├─pkgconfig (2.0.3)
    #> │ │ ├─rlang (0.4.0)
    #> │ │ └─vctrs (0.2.0)
    #> │ │   ├─backports (1.1.5)
    #> │ │   ├─ellipsis (0.3.0)
    #> │ │   │ └─rlang (0.4.0)
    #> │ │   ├─digest (0.6.21)
    #> │ │   ├─glue (1.3.1)
    #> │ │   ├─rlang (0.4.0)
    #> │ │   └─zeallot (0.1.0)
    #> │ ├─prettyunits (1.0.2)
    #> │ │ ├─magrittr (1.5)
    #> │ │ └─assertthat (0.2.1)
    #> │ ├─R6 (2.4.0)
    #> │ └─crayon (1.3.4)
    #> └─fansi (0.4.0)

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

    #> Your system is ready to build packages!
    #> Your system is ready to build packages!

    #> Installed: 17
    #> Build time:  2.8s
    #> Intall time: 1.6s

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

MIT © RStudio
