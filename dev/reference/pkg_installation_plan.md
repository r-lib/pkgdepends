# R6 class for installation from a lock file

An installation plan is similar to an installation proposal (i.e.
[pkg_installation_proposal](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)),
but it already contains the solved dependencies, complete with download
URLs.

## Usage

``` r
new_pkg_installation_plan(lockfile = "pkg.lock", config = list(), ...)
```

## Arguments

- lockfile:

  Path to the lock file to use.

- config:

  Configuration options, a named list. See
  ['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).

- ...:

  Additional arguments, passed to
  [`pkg_installation_plan$new()`](#method-new).

## Value

`new_pkg_installation_plan()` returns a `pkg_installation_plan` object.

## Details

Typically you create a `pkg_installation_plan` object with
`new_pkg_installation_plan()` and then call its `$download()` method to
download the packages and then its `$install()` method to install them.

## Super class

[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
-\> `pkg_installation_plan`

## Methods

### Public methods

- [`pkg_installation_plan$new()`](#method-pkg_installation_plan-initialize)

- [`pkg_installation_plan$resolve()`](#method-pkg_installation_plan-resolve)

- [`pkg_installation_plan$async_resolve()`](#method-pkg_installation_plan-async_resolve)

- [`pkg_installation_plan$get_solve_policy()`](#method-pkg_installation_plan-get_solve_policy)

- [`pkg_installation_plan$set_solve_policy()`](#method-pkg_installation_plan-set_solve_policy)

- [`pkg_installation_plan$solve()`](#method-pkg_installation_plan-solve)

- [`pkg_installation_plan$update()`](#method-pkg_installation_plan-update)

- [`pkg_installation_plan$update_sysreqs()`](#method-pkg_installation_plan-update_sysreqs)

- [`pkg_installation_plan$format()`](#method-pkg_installation_plan-format)

- [`pkg_installation_plan$clone()`](#method-pkg_installation_plan-clone)

Inherited methods

- [`pkg_installation_proposal$async_download()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-async_download)
- [`pkg_installation_proposal$create_lockfile()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-create_lockfile)
- [`pkg_installation_proposal$download()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-download)
- [`pkg_installation_proposal$draw()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-draw)
- [`pkg_installation_proposal$get_config()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_config)
- [`pkg_installation_proposal$get_downloads()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_downloads)
- [`pkg_installation_proposal$get_install_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_install_plan)
- [`pkg_installation_proposal$get_refs()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_refs)
- [`pkg_installation_proposal$get_resolution()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_resolution)
- [`pkg_installation_proposal$get_solution()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_solution)
- [`pkg_installation_proposal$get_sysreqs()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-get_sysreqs)
- [`pkg_installation_proposal$install()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-install)
- [`pkg_installation_proposal$install_sysreqs()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-install_sysreqs)
- [`pkg_installation_proposal$print()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-print)
- [`pkg_installation_proposal$show_solution()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-show_solution)
- [`pkg_installation_proposal$show_sysreqs()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-show_sysreqs)
- [`pkg_installation_proposal$stop_for_download_error()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-stop_for_download_error)
- [`pkg_installation_proposal$stop_for_solution_error()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.html#method-stop_for_solution_error)

------------------------------------------------------------------------

### `pkg_installation_plan$new()`

Create a new `pkg_installation_plan` object. Consider using
`new_pkg_installation_plan()` instead of calling the constructor
directly.

The returned object can be used to download and install packages,
according to the plan.

#### Usage

    pkg_installation_plan$new(
      lockfile = "pkg.lock",
      config = list(),
      remote_types = NULL
    )

#### Arguments

- `lockfile`:

  Path to the lock file to use.

- `config`:

  Configuration options. See
  ['Configuration'](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).
  It needs to include the package library to install to, in `library`.

- `remote_types`:

  Custom remote ref types, this is for advanced use, and experimental
  currently.

------------------------------------------------------------------------

### `pkg_installation_plan$resolve()`

This function is implemented for installation plans, and will error.

#### Usage

    pkg_installation_plan$resolve()

------------------------------------------------------------------------

### `pkg_installation_plan$async_resolve()`

This function is implemented for installation plans, and will error.

#### Usage

    pkg_installation_plan$async_resolve()

------------------------------------------------------------------------

### `pkg_installation_plan$get_solve_policy()`

Installation plans are already solved, and this method will return
`NA_character_`, always.

#### Usage

    pkg_installation_plan$get_solve_policy()

------------------------------------------------------------------------

### `pkg_installation_plan$set_solve_policy()`

This function is implemented for installation plans, and will error.

#### Usage

    pkg_installation_plan$set_solve_policy()

------------------------------------------------------------------------

### `pkg_installation_plan$solve()`

This function is implemented for installation plans, and will error.

#### Usage

    pkg_installation_plan$solve()

------------------------------------------------------------------------

### `pkg_installation_plan$update()`

Update the plan to the current state of the library. If the library has
not changed since the plan was created, then it does nothing. If new
packages have been installed, then it might not be necessary to download
and install all packages in the plan.

#### Usage

    pkg_installation_plan$update()

#### Details

This operation is different than creating a new proposal with the
updated library, because it uses the the packages and package versions
of the original plan. E.g. if the library has a newer version of a
package, then `$update()` will downgrade it to the version in the plan.

------------------------------------------------------------------------

### `pkg_installation_plan$update_sysreqs()`

Update information about installed and missing system requirements.

#### Usage

    pkg_installation_plan$update_sysreqs()

------------------------------------------------------------------------

### `pkg_installation_plan$format()`

Format a `pkg_installation_plan` object, typically for printing.

#### Usage

    pkg_installation_plan$format(...)

#### Arguments

- `...`:

  not used currently.

#### Returns

A character vector, each element should be a line in the printout.

------------------------------------------------------------------------

### `pkg_installation_plan$clone()`

The objects of this class are cloneable with this method.

#### Usage

    pkg_installation_plan$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
