# Package index

## Configuration

- [`current_config()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
  : pkgdepends configuration

## Package names and references

- [`pkg_refs`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  : Package references
- [`parse_pkg_refs()`](https://r-lib.github.io/pkgdepends/dev/reference/parse_pkg_refs.md)
  [`parse_pkg_ref()`](https://r-lib.github.io/pkgdepends/dev/reference/parse_pkg_refs.md)
  : Parse package location references
- [`pkg_name_check()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_name_check.md)
  : Check if an R package name is available.
- [`pkg_rx()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_rx.md)
  : A set of handy regular expressions related to R packages

## Package dependencies

- [`pkg_resolution`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
  [`pkg_resolution_result`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
  : Dependency resolution
- [`pkg_solution`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
  [`pkg_solution_result`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_solution.md)
  : The dependency solver
- [`new_pkg_deps()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md)
  : R6 class for package dependency lookup
- [`scan_deps()`](https://r-lib.github.io/pkgdepends/dev/reference/scan_deps.md)
  : Scan R code for dependent packages

## Package downloads

- [`pkg_downloads`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
  [`pkg_download_result`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_downloads.md)
  : Package downloads
- [`new_pkg_download_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md)
  : R6 class for package downloads

## Package installation

- [`install_plans`](https://r-lib.github.io/pkgdepends/dev/reference/install_plans.md)
  : Installation plans
- [`install_package_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/install_package_plan.md)
  : Perform a package installation plan
- [`new_pkg_installation_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_plan.md)
  : R6 class for installation from a lock file
- [`new_pkg_installation_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md)
  : R6 class for package download and installation.

## Package libraries

- [`lib_status()`](https://r-lib.github.io/pkgdepends/dev/reference/lib_status.md)
  : Status of packages in a library

## System requirements

- [`sysreqs_check_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md)
  [`sysreqs_fix_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md)
  : Check if installed packages have all their system requirements
- [`sysreqs_db_list()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_list.md)
  : List contents of the system requirements DB, for a platform
- [`sysreqs_db_match()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_match.md)
  : Match system requirement descriptions to the database
- [`sysreqs_db_update()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_update.md)
  : Update the cached copy of the system requirements database
- [`sysreqs_install_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_install_plan.md)
  : Create an installation plan for system requirements
- [`sysreqs_is_supported()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_is_supported.md)
  : Check if a platform has system requirements support
- [`sysreqs_list_system_packages()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_list_system_packages.md)
  : List installed system packages
- [`sysreqs_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_platforms.md)
  : List platforms with system requirements support

## Utility functions

- [`as_pkg_dependencies()`](https://r-lib.github.io/pkgdepends/dev/reference/as_pkg_dependencies.md)
  : Shorthands for dependency specifications
- [`current_r_platform()`](https://r-lib.github.io/pkgdepends/dev/reference/default_platforms.md)
  [`default_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/default_platforms.md)
  : R platforms
- [`is_valid_package_name()`](https://r-lib.github.io/pkgdepends/dev/reference/is_valid_package_name.md)
  : Check whether a package name is valid
- [`pkg_dep_types_hard()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_dep_types.md)
  [`pkg_dep_types_soft()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_dep_types.md)
  [`pkg_dep_types()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_dep_types.md)
  : Possible package dependency types
