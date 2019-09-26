
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgdepends

> Package Dependency
Resolution

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![Linux Build
Status](https://travis-ci.org/r-lib/pkgdepends.svg?branch=master)](https://travis-ci.org/r-lib/pkgdepends)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/github/r-lib/pkgdepends?svg=true)](https://ci.appveyor.com/project/gaborcsardi/pkgdepends)
[![](http://www.r-pkg.org/badges/version/pkgdepends)](http://www.r-pkg.org/pkg/pkgdepends)
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

See [‘Package references’](TODO) for details.

## Package dependencies

TODO

## Package downloads

TODO

## Package installation

TODO

## Dependency resolution

TODO

## The dependency solver

TODO

## Installation plans

TODO

## Configuration

TODO

# Related

  - [pak](https://github.com/r-lib/pak) – R package manager
  - [pkgcache](https://github.com/r-lib/pkgcache) – Metadata and pacakge
    cache
  - [devtools](https://github.com/r-lib/devtools) – Tools for R package
    developers

# License

MIT © RStudio
