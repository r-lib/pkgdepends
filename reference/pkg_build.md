# Create a binary package from an installed package

The built package will be in the current working directory.

## Usage

``` r
pkg_build(
  pkg,
  library = .libPaths()[1],
  flavor = Sys.getenv("PKG_BUILD_FLAVOR"),
  build_number = 1L
)
```

## Arguments

- pkg:

  Package name.

- library:

  Library path.

- flavor:

  Platform flavor. Defaults to the `PKG_BUILD_FLAVOR` environment
  variable. If not `NULL` or an empty string, then it is appended to the
  platform string with a dash.

- build_number:

  An integer number that is added to the file name, after the version
  number, to be able to have multiple builds for the same package
  version.

## Value

Path to the built package.

## Details

This function is currently experimental.
