# Installation plans

An installation plan contains all data that is needed to install a set
of package files. It is usually created from an [installation
proposal](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
with
[solving](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
the dependencies and
[downloading](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
the package files.

## Details

It is also possible to create an installation plan a different way. An
installation plan object must be a data frame, with at least the
following columns:

- `package`: The name of the package.

- `type`: The type of the [package
  reference](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md).

- `binary`: Whether the package is a binary package.

- `file`: Full path to the package file or directory.

- `dependencies`: A list column that lists the names of the dependent
  packages for each package.

- `needscompilation`: Whether the package needs compilation. This should
  be `FALSE` for binary packages.

For installation plans created via
[pkg_installation_proposal](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md),
the plan contains all columns from
[`pkg_download_result`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
objects, and some additional ones:

- `library`: the library the package is supposed to be installed to.

- `direct`: whether the package was directly requested or it is
  installed as a dependency.

- vignettes: whether the vignettes need to be (re)built.

- `packaged`: whether `R CMD build` was already called for the package.

## See also

[pkg_installation_proposal](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
to create install plans,
[`install_package_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/install_package_plan.md)
to install plans from any source.

## Examples

``` r
if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "pak",
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$download()
pdi$get_install_plan()
} # }
```
