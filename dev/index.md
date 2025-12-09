# pkgdepends

> Package Dependency Resolution, Downloads and Installation

pkgdepends is a toolkit for package dependencies, downloads and
installations, to be used in other packages. If you are looking for a
package manager, see [pak](https://github.com/r-lib/pak).

# Features

- Look up package dependencies recursively.
- Visualize package dependencies.
- Download packages and their dependencies.
- Install downloaded packages.
- Includes a dependency solver to find a consistent set of dependencies.
- Supports CRAN and Bioconductor packages automatically.
- Supports packages on GitHub and GitLab.
- Supports packages in git repositories.
- Supports package bundles or files on the web.
- Supports local package file and trees.
- Supports the `Remotes` entry in the `DESCRIPTION` file.
- Caches metadata and downloaded packages via
  [pkgcache](https://github.com/r-lib/pkgcache)
- Performs all downloads and HTTP queries concurrently.
- Builds and installs packages in parallel.

# Install

Install the package with:

``` r
install.packages("pkgdepends")
```

If you need the development version, install it with

``` r
pak::pak("r-lib/pkgdepends")
```

# Usage

``` r
library(pkgdepends)
```

## Package references

A package reference (ref) specifies a location from which an R package
can be obtained from. Examples:

``` R
devtools
cran::devtools
bioc::Biobase
r-lib/pkgdepends
https://github.com/r-lib/pkgdepends
local::~/works/shiny
```

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

    ## ✔ Updated metadata database: 3.25 MB in 8 files.
    ## ✔ Updating metadata database ... done
    ## r-lib/pkgcache 2.2.4.9000 [new][bld][cmp][dl] (unknown size)
    ## ├─callr 3.7.6 [new][bld][dl] (104.36 kB)
    ## │ ├─processx 3.8.6 [new][bld][cmp][dl] (165.19 kB)
    ## │ │ ├─ps 1.9.1 [new][bld][cmp][dl] (167.92 kB)
    ## │ │ └─R6 2.6.1 [new][bld][dl] (64.51 kB)
    ## │ └─R6
    ## ├─cli 3.6.5 [new][bld][cmp][dl] (640.24 kB)
    ## ├─curl 7.0.0 [new][bld][cmp][dl] (731.11 kB)
    ## ├─filelock 1.0.3 [new][bld][cmp][dl] (15.44 kB)
    ## ├─jsonlite 2.0.0 [new][bld][cmp][dl] (1.06 MB)
    ## ├─processx
    ## └─R6
    ##
    ## Key:  [new] new | [dl] download | [bld] build | [cmp] compile

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

    ## ℹ Getting 1 pkg with unknown size
    ## ✔ Got cli 3.6.5.9000 (source) (852.01 kB)

See the
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
class for details.

## Package installation

Installing or updating a set of package:

``` r
lib <- tempfile()
dir.create(lib)
pdi <- new_pkg_installation_proposal(
  "r-lib/cli",
  config = list(library = lib)
)
pdi$solve()
pdi$download()
pdi$install()
```

    ## ℹ Getting 1 pkg with unknown size
    ## ✔ Cached copy of cli 3.6.5.9000 (source) is the latest build
    ## ℹ Packaging cli 3.6.5.9000
    ## ✔ Packaged cli 3.6.5.9000 (1.7s)
    ## ℹ Building cli 3.6.5.9000
    ## ✔ Built cli 3.6.5.9000 (9s)
    ## ✔ Installed cli 3.6.5.9000 (github::r-lib/cli@3dd94c0) (1s)
    ## ✔ Summary:   1 new  in 10.1s

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

- [pak](https://github.com/r-lib/pak) – R package manager
- [pkgcache](https://github.com/r-lib/pkgcache) – Metadata and package
  cache
- [devtools](https://github.com/r-lib/devtools) – Tools for R package
  developers

## Code of Conduct

Please note that the pkgdepends project is released with a [Contributor
Code of
Conduct](https://r-lib.github.io/pkgdepends/dev/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

# License

MIT (c) RStudio
