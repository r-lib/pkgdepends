# Check if a platform has system requirements support

Check if a platform has system requirements support

## Usage

``` r
sysreqs_is_supported(sysreqs_platform = NULL)
```

## Arguments

- sysreqs_platform:

  System requirements platform. If `NULL`, then the `sysreqs_platform`
  `man_config_link("configuration option")` is used, which defaults to
  the current platform. Set this option if `.packageName` does not
  detect your platform correctly.

## Value

Logical scalar.

## See also

The `sysreqs_platform` `man_config_link("configuration option")`.

Other system requirements functions:
[`sysreqs_check_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_update.md),
[`sysreqs_install_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_install_plan.md),
[`sysreqs_list_system_packages()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_is_supported()
#> [1] TRUE
```
