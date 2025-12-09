# Create an installation plan for system requirements

This function uses
[`new_pkg_installation_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
and its methods to create an installation plan for one or more packages,
and then print their system requirements.

## Usage

``` r
sysreqs_install_plan(refs, upgrade = TRUE, config = list())
```

## Arguments

- refs:

  Packages to install.

- upgrade:

  If `TRUE`, pkgdepends will choose the latest available versions of
  packages, instead of preferring binary packages over source packages.

- config:

  Configuration options. See
  ['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).
  If it does not include `library`, then a temporary library is used,
  which is equivalent to not assuming any preinstalled packages. Pass
  `sysreqs_platform` here if you want a different platform than the one
  R is running on.

## Value

List with entries:

- `os`: character string. Operating system.

- `distribution`: character string. Linux distribution, `NA` if the OS
  is not Linux.

- `version`: character string. Distribution version, `NA` is the OS is
  not Linux.

- `pre_install`: character vector. Commands to run before the
  installation of system packages.

- `install_scripts`: character vector. Commands to run to install the
  system packages.

- `post_install`: character vector. Commands to run after the
  installation of system packages.

- `packages`: data frame. Information about the system packages that are
  needed. It has columns:

  - `sysreq`: string, cross-platform name of the system requirement.

  - `packages`: list column of character vectors. The names of the R
    packages that have this system requirement.

  - `pre_install`: list column of character vectors. Commands run before
    the package installation for this system requirement.

  - `system_packages`: list column of character vectors. Names of system
    packages to install.

  - `post_install`: list column of character vectors. Commands run after
    the package installation for this system requirement.

## See also

[`new_pkg_installation_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
to actually install packages, and potentially system requirements.

Other system requirements functions:
[`sysreqs_check_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_install_plan(
  "tidyverse",
  config = list(sysreqs_platform = "ubuntu-22.04")
)
#> $os
#> [1] "linux"
#> 
#> $distribution
#> [1] "ubuntu"
#> 
#> $version
#> [1] "22.04"
#> 
#> $pre_install
#> [1] "apt-get -y update"
#> 
#> $install_scripts
#> [1] "apt-get -y install libx11-dev libcurl4-openssl-dev libssl-dev make zlib1g-dev pandoc libfreetype6-dev libjpeg-dev libpng-dev libtiff-dev libwebp-dev libicu-dev libfontconfig1-dev libfribidi-dev libharfbuzz-dev libxml2-dev"
#> 
#> $post_install
#> character(0)
#> 
#> $packages
#> # A data frame: 16 × 5
#>    sysreq     packages  pre_install system_packages post_install
#>    <chr>      <list>    <list>      <list>          <list>      
#>  1 fontconfig <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  2 freetype   <chr [3]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  3 fribidi    <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  4 gnumake    <chr [3]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  5 harfbuzz   <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  6 libcurl    <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  7 libicu     <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  8 libjpeg    <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#>  9 libpng     <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 10 libtiff    <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 11 libwebp    <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 12 libxml2    <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 13 openssl    <chr [2]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 14 pandoc     <chr [3]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 15 x11        <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 16 zlib       <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
#> 
```
