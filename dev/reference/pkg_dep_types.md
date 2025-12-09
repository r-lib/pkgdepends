# Possible package dependency types

Hard dependencies are needed for a package to load, soft dependencies
are optional.

## Usage

``` r
pkg_dep_types_hard()

pkg_dep_types_soft()

pkg_dep_types()
```

## Value

A string vector of dependency types, capitalized.

## See also

Other package dependency utilities:
[`as_pkg_dependencies()`](https://r-lib.github.io/pkgdepends/dev/reference/as_pkg_dependencies.md)
