# pkgdepends: Package Dependency Resolution and Downloads

pkgdepends is a toolkit for package dependencies, downloads and
installations, to be used in other packages. If you are looking for a
package manager, see [pak](https://github.com/r-lib/pak).

## Features

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

## Install

Install the package with:

    install.packages("pkgdepends")

If you need the development version, install it with

    pak::pak("r-lib/pkgdepends")

## Usage

    library(pkgdepends)

### Package references

A package reference (ref) specifies a location from which an R package
can be obtained from. Examples:

    devtools
    cran::devtools
    bioc::Biobase
    r-lib/pkgdepends
    https://github.com/r-lib/pkgdepends
    local::~/works/shiny

See [“Package
references”](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
for details.

### Package dependencies

Dependencies of the development version of the cli package:

    pd <- new_pkg_deps("r-lib/pkgcache")
    pd$solve()
    pd$draw()

    ## ✔ Loading metadata database ... done
    ## r-lib/pkgcache 2.1.0.9000 ✨👷🏽🔧 
    ## ├─callr 3.7.3 ✨ ⬇ (431.00 kB)
    ## │ ├─processx 3.8.1 ✨ ⬇ (316.20 kB)
    ## │ │ ├─ps 1.7.5 ✨ ⬇ (313.92 kB)
    ## │ │ └─R6 2.5.1 ✨
    ## │ └─R6
    ## ├─cli 3.6.1 ✨ ⬇ (1.38 MB)
    ## ├─curl 5.0.0 ✨ ⬇ (777.64 kB)
    ## ├─filelock 1.0.2 ✨ ⬇ (29.58 kB)
    ## ├─jsonlite 1.8.4 ✨ ⬇ (1.13 MB)
    ## ├─prettyunits 1.1.1 ✨ ⬇ (35.23 kB)
    ## ├─processx
    ## ├─R6
    ## └─rappdirs 0.3.3 ✨ ⬇ (47.50 kB)
    ##
    ## Key:  ✨ new |  ⬇ download | 👷🏽 build | 🔧 compile

See the
[`pkg_deps`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md)
class for details.

### Package downloads

Downloading all dependencies of a package:

    pdl <- new_pkg_download_proposal("r-lib/cli")
    pdl$resolve()
    pdl$download()

    ## ℹ No downloads are needed, 1 pkg is cached
    ##

See the
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
class for details.

### Package installation

Installing or updating a set of package:

    lib <- tempfile()
    dir.create(lib)
    pdi <- new_pkg_installation_proposal(
      "r-lib/cli",
      config = list(library = lib)
    )
    pdi$solve()
    pdi$download()
    pdi$install()

    ## ℹ No downloads are needed, 1 pkg is cached
    ## ✔ Installed cli 3.6.1.9000 (github::r-lib/cli@c37f34b) (36ms)
    ## ✔ Summary:  ✨ 1 new  in 36ms

### Dependency resolution

[`pkg_deps`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
all resolve their dependencies recursively, to obtain information about
all packages needed for the specified [package
references](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md).
See [“Dependency
resolution”](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for details.

### The dependency solver

The dependency solver takes the resolution information, and works out
the exact versions of each package that must be installed, such that
version and other requirements are satisfied. See [“The dependency
solver”](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

### Installation plans

[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
can create installation plans, and then also install them. It is also
possible to import installation plans that were created by other tools.
See [“Installation
plans”](https://r-lib.github.io/pkgdepends/dev/reference/install_plans.md)
for details.

### Configuration

The details of
[`pkg_deps`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
can be tuned with a list of configuration options. See
[“Configuration”](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for details.

## Related

- [pak](https://github.com/r-lib/pak) – R package manager

- [pkgcache](https://github.com/r-lib/pkgcache) – Metadata and package
  cache

- [devtools](https://github.com/r-lib/devtools) – Tools for R package
  developers

### Code of Conduct

Please note that the pkgdepends project is released with a [Contributor
Code of
Conduct](https://r-lib.github.io/pkgdepends/dev/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

MIT (c) RStudio

## See also

Useful links:

- <https://r-lib.github.io/pkgdepends/>

- <https://github.com/r-lib/pkgdepends>

- Report bugs at <https://github.com/r-lib/pkgdepends/issues>

## Author

**Maintainer**: Gábor Csárdi <csardi.gabor@gmail.com>

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
