# Check whether a package name is valid

Check whether a package name is valid

## Usage

``` r
is_valid_package_name(nm)
```

## Arguments

- nm:

  Potential package name, string of length 1.

## Value

Logical flag. If `FALSE`, then the `reason` attribute contains a
character string, the explanation why the package name is invalid. See
examples below.

## Examples

``` r
is_valid_package_name("pak")
#> [1] TRUE
is_valid_package_name("pkg")
#> [1] FALSE
#> attr(,"reason")
#> [1] "Package name forbidden by CRAN."
is_valid_package_name("pak\u00e1ge")
#> [1] FALSE
#> attr(,"reason")
#> [1] "It can only contain ASCII characters."
is_valid_package_name("good-package")
#> [1] FALSE
#> attr(,"reason")
#> [1] "It can only contain letters, numbers and dot."
is_valid_package_name("x")
#> [1] FALSE
#> attr(,"reason")
#> [1] "It must have at least two characters."
is_valid_package_name("1stpackage")
#> [1] FALSE
#> attr(,"reason")
#> [1] "It must start with a letter."
is_valid_package_name("dots.")
#> [1] FALSE
#> attr(,"reason")
#> [1] "It must not end with a dot."
```
