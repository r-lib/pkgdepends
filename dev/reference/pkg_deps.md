# R6 class for package dependency lookup

Look up dependencies of R packages from various sources.

## Usage

``` r
new_pkg_deps(refs, ...)
```

## Arguments

- refs:

  Package names or references. See ['Package
  references'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for the syntax.

- ...:

  Additional arguments, passed to [`pkg_deps$new()`](#method-new).

## Value

`new_pkg_deps()` returns a new `pkg_deps` object.

## Details

`new_pkg_deps()` creates a new object from the `pkg_deps` class. The
advantage of `new_pkg_deps()` compared to using the pkg_deps constructor
directly is that it avoids making pkgdepends a build time dependency.

The usual steps to query package dependencies are:

1.  Create a `pkg_deps` object with `new_pkg_deps()`.

2.  Resolve all possible dependencies with
    [`pkg_deps$resolve()`](#method-resolve).

3.  Solve the dependencies, to obtain a subset of all possible
    dependencies that can be installed together, with
    [`pkg_deps$solve()`](#method-solve).

4.  Call [`pkg_deps$get_solution()`](#method-get-solution) to list the
    result of the dependency solver.

## Methods

### Public methods

- [`pkg_deps$new()`](#method-pkg_deps-new)

- [`pkg_deps$get_refs()`](#method-pkg_deps-get_refs)

- [`pkg_deps$get_config()`](#method-pkg_deps-get_config)

- [`pkg_deps$resolve()`](#method-pkg_deps-resolve)

- [`pkg_deps$async_resolve()`](#method-pkg_deps-async_resolve)

- [`pkg_deps$get_resolution()`](#method-pkg_deps-get_resolution)

- [`pkg_deps$get_solve_policy()`](#method-pkg_deps-get_solve_policy)

- [`pkg_deps$set_solve_policy()`](#method-pkg_deps-set_solve_policy)

- [`pkg_deps$solve()`](#method-pkg_deps-solve)

- [`pkg_deps$get_solution()`](#method-pkg_deps-get_solution)

- [`pkg_deps$stop_for_solution_error()`](#method-pkg_deps-stop_for_solution_error)

- [`pkg_deps$draw()`](#method-pkg_deps-draw)

- [`pkg_deps$format()`](#method-pkg_deps-format)

- [`pkg_deps$print()`](#method-pkg_deps-print)

- [`pkg_deps$clone()`](#method-pkg_deps-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `pkg_deps` object. Consider using `new_pkg_deps()` instead
of calling the constructor directly.

The returned object can be used to look up (recursive) dependencies of R
packages from various sources. To perform the actual lookup, you'll need
to call the [`resolve()`](#method-resolve) method.

#### Usage

    pkg_deps$new(
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

- `policy`:

  Solution policy. See ['The dependency
  solver'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md).

- `remote_types`:

  Custom remote ref types, this is for advanced use, and experimental
  currently.

#### Returns

A new `pkg_deps` object.

------------------------------------------------------------------------

### Method `get_refs()`

The package refs that were used to create the `pkg_deps` object.

#### Usage

    pkg_deps$get_refs()

#### Returns

A character vector of package refs that were used to create the
`pkg_deps` object.

------------------------------------------------------------------------

### Method `get_config()`

Configuration options for the `pkg_deps` object. See
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for details.

#### Usage

    pkg_deps$get_config()

#### Returns

See
['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for the configuration entries.

------------------------------------------------------------------------

### Method `resolve()`

Resolve the dependencies of the specified package references. This
usually means downloading metadata from CRAN and Bioconductor, unless
already cached, and also from GitHub if GitHub refs were included,
either directly or indirectly. See ['Dependency
resolution'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for details.

#### Usage

    pkg_deps$resolve()

#### Returns

The `pkg_deps` object itself, invisibly.

------------------------------------------------------------------------

### Method `async_resolve()`

The same as [`resolve()`](#method-resolve), but asynchronous. This
method is for advanced use.

#### Usage

    pkg_deps$async_resolve()

#### Returns

A deferred value.

------------------------------------------------------------------------

### Method `get_resolution()`

Query the result of the dependency resolution. This method can be called
after [`resolve()`](#method-resolve) has completed.

#### Usage

    pkg_deps$get_resolution()

#### Returns

A
[pkg_resolution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
object, which is also a data frame. See ['Dependency
resolution'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
for its columns.

------------------------------------------------------------------------

### Method `get_solve_policy()`

Returns the current policy of the dependency solver. See ['The
dependency
solver'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

#### Usage

    pkg_deps$get_solve_policy()

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

    pkg_deps$set_solve_policy(policy = c("lazy", "upgrade"))

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

[`solve()`](https://rdrr.io/r/base/solve.html) calls
[`resolve()`](#method-resolve) automatically, if it hasn't been called
yet.

#### Usage

    pkg_deps$solve()

#### Returns

The `pkg_deps` object itself, invisibly.

------------------------------------------------------------------------

### Method `get_solution()`

Returns the solution of the package dependencies.

#### Usage

    pkg_deps$get_solution()

#### Returns

A
[pkg_solution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
object, which is a list. See
[pkg_solution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
for details.

------------------------------------------------------------------------

### Method `stop_for_solution_error()`

Error if the dependency solver failed to find a consistent set of
packages that can be installed together.

#### Usage

    pkg_deps$stop_for_solution_error()

------------------------------------------------------------------------

### Method `draw()`

Draw a tree of package dependencies. It returns a `tree` object, see
[`cli::tree()`](https://cli.r-lib.org/reference/tree.html). Printing
this object prints the dependency tree to the screen.

#### Usage

    pkg_deps$draw()

#### Returns

A `tree` object from the cli package, see
[`cli::tree()`](https://cli.r-lib.org/reference/tree.html).

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Format a `pkg_deps` object, typically for printing.

#### Usage

    pkg_deps$format(...)

#### Arguments

- `...`:

  Not used currently.

#### Returns

A character vector, each element should be a line in the printout.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a `pkg_deps` object to the screen. The printout includes:

- The package refs.

- Whether the object has the resolved dependencies.

- Whether the resolution had errors.

- Whether the object has the solved dependencies.

- Whether the solution had errors.

- Advice on which methods to call next.

See the example below.

#### Usage

    pkg_deps$print(...)

#### Arguments

- `...`:

  not used currently.

#### Returns

The `pkg_deps` object itself, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    pkg_deps$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Method initialize()
pd <- pkg_deps$new("r-lib/pkgdepends")
pd
#> <pkg_dependencies>
#> + refs:
#>   - r-lib/pkgdepends
#> (use `$resolve()` to resolve dependencies)
#> (use `$solve()` to solve dependencies)
# Method get_refs()
pd <- new_pkg_deps(c("pak", "jsonlite"))
pd$get_refs()
#> [1] "pak"      "jsonlite"
# Method get_config()
pd <- new_pkg_deps("pak")
pd$get_config()
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
#> [1] "/tmp/Rtmp1ungDf/file24a13a1c7225"
#> 
#> ## platforms
#> <default>
#> [1] "x86_64-pc-linux-gnu-ubuntu-24.04"
#> [2] "source"                          
#> 
#> ## goal
#> <default>
#> [1] "unknown"
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
#> [1] "/tmp/Rtmp1ungDf/file24a174c95f9f"
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
#> <set>
#> [1] FALSE
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
# Method resolve()
pd <- new_pkg_deps("pak")
pd$resolve()
#> 
#> ✔ Updated metadata database: 3.64 MB in 3 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
pd$get_resolution()
#> # A data frame: 2 × 35
#>   ref   type     direct directpkg status package version license
#>   <chr> <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
#> 1 pak   standard TRUE   TRUE      OK     pak     0.9.1   GPL-3  
#> 2 pak   standard TRUE   TRUE      OK     pak     0.9.1   GPL-3  
#> # ℹ 27 more variables: needscompilation <lgl>, priority <chr>,
#> #   md5sum <chr>, sha256 <chr>, filesize <int>, built <chr>,
#> #   platform <chr>, rversion <chr>, repotype <chr>, repodir <chr>,
#> #   target <chr>, deps <list>, mirror <chr>, sources <list>,
#> #   remote <list>, error <list>, metadata <list>, extra <list>,
#> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>,
#> #   cache_status <chr>, sysreqs_packages <list>, …
# Method get_resolution()
pd <- new_pkg_deps("r-lib/pkgdepends")
pd$resolve()
pd$get_resolution()
#> # A data frame: 27 × 35
#>    ref            type  direct directpkg status package version license
#>    <chr>          <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
#>  1 r-lib/pkgdepe… gith… TRUE   TRUE      OK     pkgdep… 0.9.0.… MIT + …
#>  2 callr          stan… FALSE  FALSE     OK     callr   3.7.6   MIT + …
#>  3 cli            stan… FALSE  FALSE     OK     cli     3.6.5   MIT + …
#>  4 curl           stan… FALSE  FALSE     OK     curl    7.0.0   MIT + …
#>  5 desc           stan… FALSE  FALSE     OK     desc    1.4.3   MIT + …
#>  6 filelock       stan… FALSE  FALSE     OK     filelo… 1.0.3   MIT + …
#>  7 jsonlite       stan… FALSE  FALSE     OK     jsonli… 2.0.0   MIT + …
#>  8 lpSolve        stan… FALSE  FALSE     OK     lpSolve 5.6.23  LGPL-2 
#>  9 pkgbuild       stan… FALSE  FALSE     OK     pkgbui… 1.4.8   MIT + …
#> 10 pkgcache       stan… FALSE  FALSE     OK     pkgcac… 2.2.4   MIT + …
#> # ℹ 17 more rows
#> # ℹ 27 more variables: needscompilation <lgl>, priority <chr>,
#> #   md5sum <chr>, sha256 <chr>, filesize <int>, built <chr>,
#> #   platform <chr>, rversion <chr>, repotype <chr>, repodir <chr>,
#> #   target <chr>, deps <list>, mirror <chr>, sources <list>,
#> #   remote <list>, error <list>, metadata <list>, extra <list>,
#> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>, …
# Method get_solve_policy()
pdi <- new_pkg_deps("r-lib/pkgdepends")
pdi$get_solve_policy()
#> [1] "lazy"
pdi$set_solve_policy("upgrade")
pdi$get_solve_policy()
#> [1] "upgrade"
# Method set_solve_policy()
pdi <- new_pkg_deps("r-lib/pkgdepends")
pdi$get_solve_policy()
#> [1] "lazy"
pdi$set_solve_policy("upgrade")
pdi$get_solve_policy()
#> [1] "upgrade"
# Method solve()
pd <- new_pkg_deps("r-lib/pkgdepends")
pd$resolve()
pd$solve()
pd$get_solution()
#> <pkg_solution>
#> + result: OK
#> + refs:
#>   - r-lib/pkgdepends
#> + constraints (58):
#>   - select pkgdepends exactly once
#>   - select callr at most once
#>   - select cli at most once
#>   - select curl at most once
#>   - select desc at most once
#>   - select filelock at most once
#>   - select jsonlite at most once
#>   - select lpSolve at most once
#>   - select pkgbuild at most once
#>   - select pkgcache at most once
#>   ...
#> + solution:
#>   - R6
#>   - callr
#>   - cli
#>   - curl
#>   - desc
#>   - filelock
#>   - jsonlite
#>   - lpSolve
#>   - pkgbuild
#>   - pkgcache
#>   - processx
#>   - ps
#>   - r-lib/pkgdepends
#>   - zip
# Method get_solution()
pd <- new_pkg_deps("pkgload")
pd$resolve()
pd$solve()
pd$get_solution()
#> <pkg_solution>
#> + result: OK
#> + refs:
#>   - pkgload
#> + constraints (49):
#>   - select pkgload exactly once
#>   - select callr at most once
#>   - select cli at most once
#>   - select desc at most once
#>   - select fs at most once
#>   - select glue at most once
#>   - select lifecycle at most once
#>   - select pkgbuild at most once
#>   - select processx at most once
#>   - select ps at most once
#>   ...
#> + solution:
#>   - R6
#>   - callr
#>   - cli
#>   - desc
#>   - fs
#>   - glue
#>   - lifecycle
#>   - pkgbuild
#>   - pkgload
#>   - processx
#>   - ps
#>   - rlang
#>   - rprojroot
# Method stop_for_solution_error()
# This is an error, because the packages conflict:
pd <- new_pkg_deps(
  c("r-lib/pak", "cran::pak"),
  config = list(library = tempfile())
)
pd$resolve()
pd$solve()
pd
#> <pkg_dependencies>
#> + refs:
#>   - r-lib/pak
#>   - cran::pak
#> + has resolution (+0 dependencies)
#> + has solution
#> x has solution errors
#> (use `$get_resolution()` to see resolution results)
#> (use `$show_solution()` to see the dependencies
#> (use `$get_solution()` to see the full solution results)
# This fails:
# pd$stop_for_solution_error()
# Method draw()
pd <- new_pkg_deps("pkgload")
pd$solve()
pd$draw()
#> pkgload 1.4.1 [new][dl] (unknown size)
#> ├─cli 3.6.5 [new][dl] (unknown size)
#> ├─desc 1.4.3 [new][dl] (unknown size)
#> │ ├─R6 2.6.1 [new][dl] (unknown size)
#> │ └─cli
#> ├─fs 1.6.6 [new][dl] (unknown size)
#> ├─glue 1.8.0 [new][dl] (unknown size)
#> ├─lifecycle 1.0.4 [new][dl] (unknown size)
#> │ ├─cli
#> │ ├─glue
#> │ └─rlang 1.1.6 [new][dl] (unknown size)
#> ├─pkgbuild 1.4.8 [new][dl] (unknown size)
#> │ ├─R6
#> │ ├─callr 3.7.6 [new][dl] (unknown size)
#> │ │ ├─R6
#> │ │ └─processx 3.8.6 [new][dl] (unknown size)
#> │ │   ├─R6
#> │ │   └─ps 1.9.1 [new][dl] (unknown size)
#> │ ├─cli
#> │ ├─desc
#> │ └─processx
#> ├─processx
#> ├─rlang
#> └─rprojroot 2.1.1 [new][dl] (unknown size)
#> 
#> Key:  [new] new | [dl] download
# Method print()
pd <- new_pkg_deps("r-lib/pkgdepends")
pd
#> <pkg_dependencies>
#> + refs:
#>   - r-lib/pkgdepends
#> (use `$resolve()` to resolve dependencies)
#> (use `$solve()` to solve dependencies)

pd$resolve()
pd
#> <pkg_dependencies>
#> + refs:
#>   - r-lib/pkgdepends
#> + has resolution (+13 dependencies)
#> (use `$get_resolution()` to see resolution results)
#> (use `$solve()` to solve dependencies)

pd$solve()
pd
#> <pkg_dependencies>
#> + refs:
#>   - r-lib/pkgdepends
#> + has resolution (+13 dependencies)
#> + has solution
#> (use `$get_resolution()` to see resolution results)
#> (use `$show_solution()` to see the dependencies
#> (use `$get_solution()` to see the full solution results)
#> (use `$draw()` to draw the dependency tree)
```
