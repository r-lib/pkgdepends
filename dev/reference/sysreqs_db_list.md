# List contents of the system requirements DB, for a platform

It also tries to update the system dependency database, if it is
outdated. (I.e. older than allowed in the `metadata_update_after`
`man_config_link("configuration option")`.

## Usage

``` r
sysreqs_db_list(sysreqs_platform = NULL)
```

## Arguments

- sysreqs_platform:

  System requirements platform. If `NULL`, then the `sysreqs_platform`
  `man_config_link("configuration option")` is used, which defaults to
  the current platform. Set this option if `.packageName` does not
  detect your platform correctly.

## Value

Data frame with columns:

- `name`: cross platform system dependency name in the database.

- `patterns`: one or more regular expressions to match to
  `SystemRequirements` fields.

- `packages`: one or more system package names to install.

- `pre_install`: command(s) to run before installing the packages.

- `post_install`:: command(s) to run after installing the packages.

## See also

Other system requirements functions:
[`sysreqs_check_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md),
[`sysreqs_db_match()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_update.md),
[`sysreqs_install_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_install_plan.md),
[`sysreqs_is_supported()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_db_list(sysreqs_platform = "ubuntu-22.04")
#> # A data frame: 127 × 5
#>    name       patterns  packages  pre_install post_install
#>    <chr>      <list>    <list>    <list>      <list>      
#>  1 Abseil     <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  2 QuantLib   <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  3 apparmor   <chr [2]> <chr [1]> <NULL>      <NULL>      
#>  4 atk        <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  5 automake   <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  6 berkeleydb <chr [2]> <chr [1]> <NULL>      <NULL>      
#>  7 blender    <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  8 blosc      <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  9 boost      <chr [1]> <chr [1]> <NULL>      <NULL>      
#> 10 bowtie2    <chr [1]> <chr [1]> <NULL>      <NULL>      
#> # ℹ 117 more rows
```
