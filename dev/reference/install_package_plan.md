# Perform a package installation plan

See ['Installation
plans'](https://r-lib.github.io/pkgdepends/dev/reference/install_plans.md)
for the details and the format.

## Usage

``` r
install_package_plan(
  plan,
  lib = .libPaths()[[1]],
  num_workers = 1,
  cache = NULL
)
```

## Arguments

- plan:

  Package plan object, a data frame, see ['Installation
  plans'](https://r-lib.github.io/pkgdepends/dev/reference/install_plans.md)
  for the format.

- lib:

  Library directory to install to.

- num_workers:

  Number of worker processes to use.

- cache:

  Package cache to use, or `NULL`.

## Value

Information about the installation process.
