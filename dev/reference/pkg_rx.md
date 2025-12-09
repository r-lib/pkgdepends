# A set of handy regular expressions related to R packages

If you use these in R, make sure you specify `perl = TRUE`, see
[`base::grep()`](https://rdrr.io/r/base/grep.html).

## Usage

``` r
pkg_rx()
```

## Value

A named list of strings.

## Details

Currently included:

- `pkg_name`: A valid package name.

- `type_cran`: A `cran::` package reference.

- `type_bioc`: A `bioc::` package reference.

- `type_standard`: A `standard::` package reference.

- `type_github`: A `github::` package reference.

- `type_git`: A `git::` package reference.

- `type_local`: A `local::` package reference.

- `type_deps`: A `deps::` package reference.

- `type_installed`: An `installed::` package reference.

- `github_username`: A GitHub username.

- `github_repo`: A GitHub repository name.

- `github_url`: A GitHub URL.

## Examples

``` r
pkg_rx()
#> $pkg_name
#> [1] "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
#> 
#> $type_cran
#> [1] "^(?:cran::)?(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])(?:@(?:(?:(?<atleast>>=)?(?<version>[0-9]+[-\\.][0-9]+(?:[-\\.][0-9]+)*|current|last))))?$"
#> 
#> $type_bioc
#> [1] "^(?:bioc::)?(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])(?:@(?:(?:(?<atleast>>=)?(?<version>[0-9]+[-\\.][0-9]+(?:[-\\.][0-9]+)*|current|last))))?$"
#> 
#> $type_standard
#> [1] "^(?:standard::)?(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])(?:@(?:(?:(?<atleast>>=)?(?<version>[0-9]+[-\\.][0-9]+(?:[-\\.][0-9]+)*|current|last))))?$"
#> 
#> $type_github
#> [1] "^(?:(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])=)?(?:github::)?(?<username>(?:[a-zA-Z\\d](?:[a-zA-Z\\d-]){0,38}))/(?<repo>[^/@#]+)(?:/(?<subdir>(?:[^@#]*[^@#/])/?))?(?:(?:(?:@(?<commitish>[^*].*)))|(?:(?:#(?<pull>[0-9]+)))|(?:(?:@(?<release>[*]release))))?$"
#> 
#> $type_git
#> [1] "^(?:(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])=)?(?:git::)(?:(?<protocol>[^/]*)://)?(?<host>[^/]+)(?<path>[^@]*/)(?<repo>[^/@]*)(?:@(?<commitish>.*))?"
#> 
#> $type_local
#> [1] "^(?:(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])=)?(?|local::(?<path>.*)|(?<path>(?:/|\\\\|~|[.]/|[.]\\\\|[.]$).*))$"
#> 
#> $type_deps
#> [1] "^(?:deps::)(?<path>.*)$"
#> 
#> $type_installed
#> [1] "^(?:installed::)?(?<library>.*)/(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])$"
#> 
#> $github_username
#> [1] "(?<username>(?:[a-zA-Z\\d](?:[a-zA-Z\\d-]){0,38}))"
#> 
#> $github_repo
#> [1] "(?<repo>[^/@#]+)"
#> 
#> $github_url
#> [1] "^(?:(?<package>[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9])=)?(?:github::)?(?:(?:https?://)|(?:(?:ssh://|[^@]+@)))(?:[^/:]+)[/:](?<username>(?:[a-zA-Z\\d](?:[a-zA-Z\\d-]){0,38}))/(?<repo>[^/@#]+?)(?<subdir>)(?:[.]git)?(?:/(?:(?:(?:tree|commit|releases/tag)/(?<commitish>.+$))|(?:pull/(?<pull>.+$))|(?:releases/)(?<release>.+$)))?$"
#> 
```
