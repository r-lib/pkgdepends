# R6 class for package download and installation.

Download and install R packages, with their dependencies, from various
sources.

## Usage

``` r
new_pkg_installation_proposal(refs, config = list(), ...)
```

## Arguments

- refs:

  Package names or references. See ['Package
  references'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for the syntax.

- config:

  Configuration options, a named list. See
  ['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).
  If it does not include `library`, then `.libPaths()[1]` is added as
  `library`.

- ...:

  Additional arguments, passed to
  [`pkg_installation_proposal$new()`](#method-new).

## Value

`new_pkg_installation_proposal()` returns a new
`pkg_installation_proposal` object.

## Details

`new_pkg_installation_proposal()` creates a new object from the
`pkg_installation_proposal` class. The advantage of
`new_pkg_installation_proposal()` compared to using the
pkg_installation_proposal constructor directly is that it avoids making
pkgdepends a build time dependency.

Typical workflow to install a set of packages:

1.  Create a `pkg_installation_proposal` object with
    `new_pkg_installation_proposal()`.

2.  Resolve all possible dependencies with
    [`pkg_installation_proposal$resolve()`](#method-resolve).

3.  Solve the package dependencies, to get an installation plan, with
    [`pkg_installation_proposal$solve()`](#method-solve).

4.  Download all files with
    [`pkg_installation_proposal$download()`](#method-download).

5.  Install the downloaded files with
    [`pkg_installation_proposal$install()`](#methods-install).

## Methods

### Public methods

- [`pkg_installation_proposal$new()`](#method-pkg_installation_proposal-new)

- [`pkg_installation_proposal$get_refs()`](#method-pkg_installation_proposal-get_refs)

- [`pkg_installation_proposal$get_config()`](#method-pkg_installation_proposal-get_config)

- [`pkg_installation_proposal$resolve()`](#method-pkg_installation_proposal-resolve)

- [`pkg_installation_proposal$async_resolve()`](#method-pkg_installation_proposal-async_resolve)

- [`pkg_installation_proposal$get_resolution()`](#method-pkg_installation_proposal-get_resolution)

- [`pkg_installation_proposal$get_solve_policy()`](#method-pkg_installation_proposal-get_solve_policy)

- [`pkg_installation_proposal$set_solve_policy()`](#method-pkg_installation_proposal-set_solve_policy)

- [`pkg_installation_proposal$solve()`](#method-pkg_installation_proposal-solve)

- [`pkg_installation_proposal$get_solution()`](#method-pkg_installation_proposal-get_solution)

- [`pkg_installation_proposal$show_solution()`](#method-pkg_installation_proposal-show_solution)

- [`pkg_installation_proposal$get_sysreqs()`](#method-pkg_installation_proposal-get_sysreqs)

- [`pkg_installation_proposal$show_sysreqs()`](#method-pkg_installation_proposal-show_sysreqs)

- [`pkg_installation_proposal$stop_for_solution_error()`](#method-pkg_installation_proposal-stop_for_solution_error)

- [`pkg_installation_proposal$create_lockfile()`](#method-pkg_installation_proposal-create_lockfile)

- [`pkg_installation_proposal$draw()`](#method-pkg_installation_proposal-draw)

- [`pkg_installation_proposal$download()`](#method-pkg_installation_proposal-download)

- [`pkg_installation_proposal$async_download()`](#method-pkg_installation_proposal-async_download)

- [`pkg_installation_proposal$get_downloads()`](#method-pkg_installation_proposal-get_downloads)

- [`pkg_installation_proposal$stop_for_download_error()`](#method-pkg_installation_proposal-stop_for_download_error)

- [`pkg_installation_proposal$install()`](#method-pkg_installation_proposal-install)

- [`pkg_installation_proposal$install_sysreqs()`](#method-pkg_installation_proposal-install_sysreqs)

- [`pkg_installation_proposal$get_install_plan()`](#method-pkg_installation_proposal-get_install_plan)

- [`pkg_installation_proposal$format()`](#method-pkg_installation_proposal-format)

- [`pkg_installation_proposal$print()`](#method-pkg_installation_proposal-print)

- [`pkg_installation_proposal$clone()`](#method-pkg_installation_proposal-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `pkg_installation_proposal` object. Consider using
`new_pkg_installation_proposal()` instead of calling the constructor
directly.

The returned object can be used to look up (recursive) dependencies of R
packages from various sources, and then download and install the package
files.

#### Usage

    pkg_installation_proposal$new(
      refs,
      config = list(),
      policy = c("lazy", "upgrade"),
      remote_types = NULL
    )

#### Arguments

- `refs`:

  Package names or references. See ['Package
  references'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for the syntax.

- `config`:

  Configuration options, a named list. See
  ['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).
  It needs to include the package library to install to, in `library`.

- `policy`:

  Solution policy. See ['The dependency
  solver'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md).

- `remote_types`:

  Custom remote ref types, this is for advanced use, and experimental
  currently.

------------------------------------------------------------------------

### Method `get_refs()`

The package refs that were used to create the
`pkg_installation_proposal` object.

#### Usage

    pkg_installation_proposal$get_refs()

#### Returns

A character vector of package refs that were used to create the
`pkg_installation_proposal` object.

------------------------------------------------------------------------

### Method `get_config()`

Configuration options for the `pkg_installation_proposal` object. See
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for details.

#### Usage

    pkg_installation_proposal$get_config()

#### Returns

Named list. See
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for the configuration options.

------------------------------------------------------------------------

### Method `resolve()`

Resolve the dependencies of the specified package references. This
usually means downloading metadata from CRAN and Bioconductor, unless
already cached, and also from GitHub if GitHub refs were included,
either directly or indirectly. See ['Dependency
resolution'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for details.

#### Usage

    pkg_installation_proposal$resolve()

#### Returns

The `pkg_installation_proposal` object, invisibly.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "pak",
      config = list(library = tempfile())
    )

    pdi$resolve()
    pdi$get_resolution()
    }

------------------------------------------------------------------------

### Method `async_resolve()`

The same as [`resolve()`](#method-resolve), but asynchronous. This
method is for advanced use.

#### Usage

    pkg_installation_proposal$async_resolve()

#### Returns

A deferred value.

------------------------------------------------------------------------

### Method `get_resolution()`

Query the result of the dependency resolution. This method can be called
after [`resolve()`](#method-resolve) has completed.

#### Usage

    pkg_installation_proposal$get_resolution()

#### Returns

A
[pkg_resolution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
object, which is also a data frame. See ['Dependency
resolution'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for its columns.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "r-lib/pkgdepends",
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$get_resolution()
    }

------------------------------------------------------------------------

### Method `get_solve_policy()`

Returns the current policy of the dependency solver. See ['The
dependency
solver'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

#### Usage

    pkg_installation_proposal$get_solve_policy()

#### Returns

A character vector of length one.

------------------------------------------------------------------------

### Method `set_solve_policy()`

Set the current policy of the dependency solver. If the object already
contains a solution and the new policy is different than the old policy,
then the solution is deleted. See ['The dependency
solver'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

#### Usage

    pkg_installation_proposal$set_solve_policy(policy = c("lazy", "upgrade"))

#### Arguments

- `policy`:

  Policy to set.

------------------------------------------------------------------------

### Method [`solve()`](https://rdrr.io/r/base/solve.html)

Solve the package dependencies. Out of the resolved dependencies, it
works out a set of packages, that can be installed together to create a
functional installation. The set includes all directly specified
packages, and all required (or suggested, depending on the
configuration) packages as well. It includes every package at most once.
See ['The dependency
solver'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

#### Usage

    pkg_installation_proposal$solve()

#### Returns

The `pkg_installation_proposal` object itself, invisibly.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "r-lib/pkgdepends",
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$get_solution()
    }

------------------------------------------------------------------------

### Method `get_solution()`

Returns the solution of the package dependencies.

#### Usage

    pkg_installation_proposal$get_solution()

#### Returns

A
[pkg_solution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
object, which is a list. See
[pkg_solution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "r-lib/pkgdepends",
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$get_solution()
    }

------------------------------------------------------------------------

### Method `show_solution()`

Show the solution on the screen.

#### Usage

    pkg_installation_proposal$show_solution(key = FALSE)

#### Arguments

- `key`:

  Whether to show the key to the package list annotation.

#### Returns

A
[pkg_solution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
object, which is a list. See
[pkg_solution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "r-lib/pkgdepends",
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$get_solution()
    pdi$show_solution()
    }

------------------------------------------------------------------------

### Method `get_sysreqs()`

Query and categorize system requirements.

#### Usage

    pkg_installation_proposal$get_sysreqs()

------------------------------------------------------------------------

### Method `show_sysreqs()`

Show system requirements for the packages in the solution.

#### Usage

    pkg_installation_proposal$show_sysreqs()

------------------------------------------------------------------------

### Method `stop_for_solution_error()`

Error if the dependency solver failed to find a consistent set of
packages that can be installed together.

#### Usage

    pkg_installation_proposal$stop_for_solution_error()

#### Examples

    \dontrun{
    # This is an error, because the packages conflict:
    pdi <- new_pkg_installation_proposal(
      c("r-lib/pak", "cran::pak"),
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi
    # This fails:
    # pdi$stop_for_solution_error()
    }

------------------------------------------------------------------------

### Method `create_lockfile()`

Create a lock file that contains the information to perform the
installation later, possibly in another R session.

#### Usage

    pkg_installation_proposal$create_lockfile(path = "pkg.lock", version = 1)

#### Arguments

- `path`:

  Name of the lock file. The default is `pkg.lock` in the current
  working directory.

- `version`:

  Only version 1 is supported currently.

#### Details

Note, since the URLs of CRAN and most CRAN-like repositories change over
time, in practice you cannot perform the plan of the lock file *much*
later. For example, binary packages of older package version are
removed, and won't be found.

Similarly, for `url::` remote types, the URL might hold an updated
version of the package, compared to when the lock file was created.
Should this happen, pkgdepends prints a warning, but it will try to
continue the installation. The installation might fail if the updated
package has different (e.g. new) dependencies.

Currently the intended use case of lock files in on CI systems, to
facilitate caching. The (hash of the) lock file provides a good key for
caching systems.

------------------------------------------------------------------------

### Method `draw()`

Draw a tree of package dependencies. It returns a `tree` object, see
[`cli::tree()`](https://cli.r-lib.org/reference/tree.html). Printing
this object prints the dependency tree to the screen.

#### Usage

    pkg_installation_proposal$draw()

#### Returns

A `tree` object from the cli package, see
[`cli::tree()`](https://cli.r-lib.org/reference/tree.html).

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "pak",
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$draw()
    }

------------------------------------------------------------------------

### Method `download()`

Download all packages that are part of the solution. It uses the package
cache in the pkgcache package by default, to avoid downloads if
possible.

#### Usage

    pkg_installation_proposal$download()

#### Returns

The `pkg_installation_proposal` object itself, invisibly.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      c("r-lib/pak", "cran::pak"),
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$download()
    pdi$get_downloads()
    }

------------------------------------------------------------------------

### Method `async_download()`

The same as [`download()`](#method-download), but asynchronous. This
method is for advanced use.

#### Usage

    pkg_installation_proposal$async_download()

#### Returns

A deferred value.

------------------------------------------------------------------------

### Method `get_downloads()`

Returns the summary of the package downloads.

#### Usage

    pkg_installation_proposal$get_downloads()

#### Returns

A
[pkg_download_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
object, which is a list. See
[pkg_download_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
for details.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      c("r-lib/pak", "cran::pak"),
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$download()
    pdi$get_downloads()
    }

------------------------------------------------------------------------

### Method `stop_for_download_error()`

Throw and error if the some of the downloads have failed for the most
recent [`pkg_installation_proposal$download()`](#method-download) call.

#### Usage

    pkg_installation_proposal$stop_for_download_error()

------------------------------------------------------------------------

### Method `install()`

Install the downloaded packages. It calls
[`install_package_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/install_package_plan.md).

#### Usage

    pkg_installation_proposal$install()

#### Returns

The return value of
[`install_package_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/install_package_plan.md).

------------------------------------------------------------------------

### Method `install_sysreqs()`

Install system requirements. It does nothing if system requirements are
turned off. Create an installation plan for the downloaded packages.

#### Usage

    pkg_installation_proposal$install_sysreqs()

------------------------------------------------------------------------

### Method `get_install_plan()`

#### Usage

    pkg_installation_proposal$get_install_plan()

#### Returns

An installation plan, see ['Installation
plans'](https://r-lib.github.io/pkgdepends/dev/reference/install_plans.md)
for the format.

#### Examples

    \dontrun{
    pdi <- new_pkg_installation_proposal(
      "pak",
      config = list(library = tempfile())
    )
    pdi$resolve()
    pdi$solve()
    pdi$download()
    pdi$get_install_plan()
    }

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Format a `pkg_installation_proposal` object, typically for printing.

#### Usage

    pkg_installation_proposal$format(...)

#### Arguments

- `...`:

  not used currently.

#### Returns

A character vector, each element should be a line in the printout.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a `pkg_installation_proposal` object to the screen.

The printout includes:

- The package refs.

- The policy of the dependency solver.

- Whether the object has the solved dependencies.

- Whether the solution had errors.

- Whether the object has downloads.

- Whether the downloads had errors.

- Advice on which methods to call next.

See the example below.

#### Usage

    pkg_installation_proposal$print(...)

#### Arguments

- `...`:

  not used currently.

#### Returns

The `pkg_installation_proposal` object itself, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    pkg_installation_proposal$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "pak",
  config = list(library = tempfile())
)
pdi

pdi$resolve()
pdi

pdi$solve()
pdi

pdi$download()
pdi
} # }
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile()))
#> ℹ Creating library directory: /tmp/Rtmp1ungDf/file24a17038493b
pdi
#> <pkg_installation_proposal>
#> + refs:
#>   - r-lib/pkgdepends
#> + solution policy: lazy
#> (use `$solve()` to solve dependencies)
pdi <- new_pkg_installation_proposal("r-lib/pkgdepends")
pdi$get_refs()
#> [1] "r-lib/pkgdepends"
pdi <- new_pkg_installation_proposal(
  "pak",
  config = list(library = tempfile())
)
#> ℹ Creating library directory: /tmp/Rtmp1ungDf/file24a11f4bd446
pdi$get_config()
#> # pkg config
#> ## build_vignettes
#> <default>
#> [1] FALSE
#> 
#> ## sysreqs_verbose
#> <default>
#> [1] TRUE
#> 
#> ## sysreqs_db_update
#> <default>
#> [1] TRUE
#> 
#> ## metadata_cache_dir
#> <default>
#> [1] "/tmp/Rtmp1ungDf/file24a14302358a"
#> 
#> ## platforms
#> <default>
#> [1] "x86_64-pc-linux-gnu-ubuntu-24.04"
#> [2] "source"                          
#> 
#> ## goal
#> <set>
#> [1] "install"
#> 
#> ## r_versions
#> <default>
#> [1] "4.5.2"
#> 
#> ## cache_dir
#> <default>
#> [1] "/tmp/Rtmp1ungDf/file24a17ece72a6"
#> 
#> ## library
#> <set>
#> [1] "/tmp/Rtmp1ungDf/file24a11f4bd446"
#> 
#> ## metadata_update_after
#> <default>
#> Time difference of 24 hours
#> 
#> ## include_linkingto
#> <default>
#> [1] FALSE
#> 
#> ## sysreqs_rspm_repo_id
#> <default>
#> [1] "1"
#> 
#> ## sysreqs_update
#> <default>
#> [1] TRUE
#> 
#> ## package_cache_dir
#> <default>
#> NULL
#> 
#> ## sysreqs_rspm_url
#> <default>
#> [1] "https://packagemanager.posit.co"
#> 
#> ## sysreqs_sudo
#> <default>
#> [1] TRUE
#> 
#> ## sysreqs_db_update_timeout
#> <default>
#> Time difference of 60 secs
#> 
#> ## sysreqs_lookup_system
#> <default>
#> [1] TRUE
#> 
#> ## git_submodules
#> <default>
#> [1] FALSE
#> 
#> ## sysreqs_platform
#> <default>
#> [1] "x86_64-pc-linux-gnu-ubuntu-24.04"
#> 
#> ## dependencies
#> <default>
#> [1] "Depends"   "Imports"   "LinkingTo"
#> 
#> ## sysreqs
#> <default>
#> [1] TRUE
#> 
#> ## sysreqs_dry_run
#> <default>
#> [1] FALSE
#> 
#> ## windows_archs
#> <default>
#> [1] "prefer-x64"
#> 
#> ## use_bioconductor
#> <default>
#> [1] TRUE
#> 
#> ## cran_mirror
#> <default>
#>                       CRAN 
#> "https://cran.rstudio.com" 
#> 
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile())
)
#> ℹ Creating library directory: /tmp/Rtmp1ungDf/file24a14b0bd9aa
pdi$get_solve_policy()
#> [1] "lazy"
pdi$set_solve_policy("upgrade")
pdi$get_solve_policy()
#> [1] "upgrade"
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile())
)
#> ℹ Creating library directory: /tmp/Rtmp1ungDf/file24a173dc0ab7
pdi$get_solve_policy()
#> [1] "lazy"
pdi$set_solve_policy("upgrade")
pdi$get_solve_policy()
#> [1] "upgrade"
# Method print
pdi <- new_pkg_installation_proposal(
  "pak",
  config = list(library = tempfile())
)
#> ℹ Creating library directory: /tmp/Rtmp1ungDf/file24a19583825
pdi
#> <pkg_installation_proposal>
#> + refs:
#>   - pak
#> + solution policy: lazy
#> (use `$solve()` to solve dependencies)

pdi$resolve()
pdi
#> <pkg_installation_proposal>
#> + refs:
#>   - pak
#> + solution policy: lazy
#> (use `$solve()` to solve dependencies)

pdi$solve()
pdi
#> <pkg_installation_proposal>
#> + refs:
#>   - pak
#> + solution policy: lazy
#> + has solution
#> (use `$download()` to download packages)
#> (use `$show_solution()` to see the packages to install
#> (use `$get_solution()` to see the full solution results)
#> (use `$draw()` to draw the dependency tree)
#> (use `$create_lockfile()` to write a lock file)

pdi$download()
#> ℹ Getting 1 pkg with unknown size
#> ✔ Got pak 0.9.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (7.03 MB)
pdi
#> <pkg_installation_proposal>
#> + refs:
#>   - pak
#> + solution policy: lazy
#> + has solution
#> + has downloads
#> (use `$show_solution()` to see the packages to install
#> (use `$get_solution()` to see the full solution results)
#> (use `$draw()` to draw the dependency tree)
#> (use `$create_lockfile()` to write a lock file)
#> (use `$get_downloads()` to get download data)
#> (use `$get_install_plan()` to get the installation plan)
#> (use `$install()` to install the packages)

## ------------------------------------------------
## Method `pkg_installation_proposal$resolve`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "pak",
  config = list(library = tempfile())
)

pdi$resolve()
pdi$get_resolution()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$get_resolution`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile())
)
pdi$resolve()
pdi$get_resolution()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$solve`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$get_solution()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$get_solution`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$get_solution()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$show_solution`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "r-lib/pkgdepends",
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$get_solution()
pdi$show_solution()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$stop_for_solution_error`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# This is an error, because the packages conflict:
pdi <- new_pkg_installation_proposal(
  c("r-lib/pak", "cran::pak"),
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi
# This fails:
# pdi$stop_for_solution_error()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$draw`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  "pak",
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$draw()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$download`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  c("r-lib/pak", "cran::pak"),
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$download()
pdi$get_downloads()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$get_downloads`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pdi <- new_pkg_installation_proposal(
  c("r-lib/pak", "cran::pak"),
  config = list(library = tempfile())
)
pdi$resolve()
pdi$solve()
pdi$download()
pdi$get_downloads()
} # }

## ------------------------------------------------
## Method `pkg_installation_proposal$get_install_plan`
## ------------------------------------------------

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
