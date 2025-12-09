# Changelog

## pkgdepends (development version)

## pkgdepends 0.9.0

CRAN release: 2025-05-27

- New function
  [`scan_deps()`](https://r-lib.github.io/pkgdepends/dev/reference/scan_deps.md)
  to auto-detect package dependencies from R code. `deps::.`
  automatically uses detected dependencies now if no `DESCRIPTION` file
  is found.

- pkgdepends now does not restore ownership when uncompressing `.tar.gz`
  files. This works better on sshfs and other file systems that do not
  have proper Unix permissions.

- The dependency solver now uses better heuristics, that works better
  (=faster) with multiple repositories with large overlaps
  ([\#392](https://github.com/r-lib/pkgdepends/issues/392)).

## pkgdepends 0.8.0

CRAN release: 2024-09-12

- pkgdepends now supports `gitlab::` package sources better, by adding
  explicit syntax to specify subdirectories
  ([\#353](https://github.com/r-lib/pkgdepends/issues/353),
  [@dgkf](https://github.com/dgkf)).

- `gitlab::` and `git::` package sources now support git submodules if
  the `git-submodules` configuration option is set to `TRUE`. See
  [`?"pkgdepends-config"`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
  ([\#354](https://github.com/r-lib/pkgdepends/issues/354)).

- The new `?ignore-unavailable` parameter makes it easy to ignore soft
  dependencies that are unavailable
  (<https://github.com/r-lib/pak/issues/606>).

- pkgdepends now automatically ignores soft dependencies that have an
  incompatible OS type (`OS_type` entry in `DESCRIPTION`) when
  installing packages.

## pkgdepends 0.7.2

CRAN release: 2024-03-17

- pkgdepends now supports the `*` wildcard for parameter specifications,
  for parameters applied to all packages. E.g. `*=?source` means
  compiling all packages from source.

## pkgdepends 0.7.1

CRAN release: 2023-12-10

- pkgdepends now does not import the glue, rprojroot and prettyunits
  packages.

- pkgdepends can now handle the case when `Config/Needs/*` dependencies
  are requested for package from a repository.

## pkgdepends 0.7.0

CRAN release: 2023-11-10

- pkgdepends now correctly resolves the latest GitHub release with the
  `@*release` notation ([@pawelru](https://github.com/pawelru),
  [\#321](https://github.com/r-lib/pkgdepends/issues/321),
  [\#275](https://github.com/r-lib/pkgdepends/issues/275)).

- pkgdepends now correctly handles having multiple instances of the same
  package in the metadata, with different R version requirements
  (<https://github.com/r-lib/pak/issues/534>,
  [\#331](https://github.com/r-lib/pkgdepends/issues/331),
  <https://github.com/r-lib/pak/issues/538>).

- `git::` package references work better now for Azure DevOps
  ([@jameslairdsmith](https://github.com/jameslairdsmith),
  [\#333](https://github.com/r-lib/pkgdepends/issues/333),
  [\#342](https://github.com/r-lib/pkgdepends/issues/342)).

- pkgdepends now does a better job at accepting installed packages, and
  avoids reinstalling more packages than needed when using a lock file
  (<https://github.com/r-lib/actions/issues/759>,
  [\#338](https://github.com/r-lib/pkgdepends/issues/338)).

## pkgdepends 0.6.0

CRAN release: 2023-07-18

- Many system requirements improvements:

  - New functions:
    - [`sysreqs_check_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md):
      check if all required system packages are installed,
    - [`sysreqs_fix_installed()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_check_installed.md):
      install missing system packages,
    - [`sysreqs_db_list()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_list.md):
      list system requirements database,
    - [`sysreqs_db_match()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_match.md):
      match `SystemRrequirements` field(s) to database,
    - [`sysreqs_db_update()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_db_update.md):
      update system requirements database,
    - [`sysreqs_install_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_install_plan.md):
      look up system requirements for a package and its dependencies,
    - [`sysreqs_is_supported()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_is_supported.md):
      check if pkgdepends supports system requirements on your platform,
    - [`sysreqs_list_system_packages()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_list_system_packages.md):
      list installed system packages,
    - [`sysreqs_platforms()`](https://r-lib.github.io/pkgdepends/dev/reference/sysreqs_platforms.md):
      list supported platforms.
    - New `pkg_installation_proposal` methods: `get_sysreqs()`,
      `show_sysreqs()` and `update_sysreqs()`.
  - The output of `$show_solution()` now includes system requirements.
  - New `sysreqs_platform` configuration option.
  - pkgdepends now looks up system requirements asynchronously, during
    dependency resolution.
  - pkgdepends now does not reinstall system requirements by default, if
    they are already installed. (You can force a reinstall/upgrade with
    the `sysreqs_update` configuration option.)

- New `gitlab::` package source to install packages from GitLab
  ([\#315](https://github.com/r-lib/pkgdepends/issues/315)).

- pkgdepends now correctly parses multiple `git::` packages at once
  ([\#318](https://github.com/r-lib/pkgdepends/issues/318)).

- Fix `@*release` reference for the latest release.

- `git::` package sources now support version 1 of the git protocol.
  E.g. the Bioconductor git repositories now work:
  `git::https://git.bioconductor.org/packages/limma`
  ([\#314](https://github.com/r-lib/pkgdepends/issues/314)).

- The `platforms` config parameter now works correctly with `deps::`
  package sources (<https://github.com/r-lib/pak/issues/522>).

- New `include_linkingto` config parameter to always include `LinkingTo`
  packages in the solution, even for binaries
  ([\#485](https://github.com/r-lib/pkgdepends/issues/485)).

- [`pkg_name_check()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_name_check.md)
  now does not include Acromine results, because the web site was
  unstable.

## pkgdepends 0.5.0

CRAN release: 2023-04-19

- pkgdepends now support git repositories as package references. E.g.
  `git::https://github.com/r-lib/pak.git`.

- pkgdepends now supports versioned CRAN packages, e.g. `dplyr@1.1.1`
  will always install dplyr 1.1.1. Note that only CRAN packages are
  supported, Bioconductor packages are not (yet).

- pkgdepends now has an alternative system requirements lookup
  implementation. It supports Fedora and Debian systems as well, in
  addition to Debian, Ubuntu, SUSE and RedHat derivatives. You can
  switch to this implementation by setting the `R_PKG_SYSREQS2`
  environment variable to `true`.

- pkgdepends now does a better job looking up dependencies for
  hand-selected dependency types. E.g. `dependencies = "LinkingTo"`.

- pkgdepends now removes `?ignore`-d packages from dependencies, and
  uses the correct version comparison for `?ignore-before.r`
  (<https://github.com/r-lib/actions/issues/708>).

- pkgdepends now does not fail for circular soft dependencies
  ([\#306](https://github.com/r-lib/pkgdepends/issues/306)).

- pkgdepends now reports dependency solver failures better in some cases
  ([\#305](https://github.com/r-lib/pkgdepends/issues/305),
  <https://github.com/r-lib/pak/issues/474>).

- pkgdepends now uses locally built CRAN binaries from the cache.

## pkgdepends 0.4.0

CRAN release: 2022-12-17

- pkgdepends has much improved and more informative error messages now.
  This work is not yet finished, so if you find an unclear error
  message, please open an issue. Thank you!

- The solver is now more robust for non-canonical input
  (e.g. `DESCRIPTION` files)
  (<https://github.com/r-lib/pak/issues/423>).

- Better installation output. Standard output and error are now
  collected together
  (<https://github.com/r-lib/pkgdepends/commit/0669f0f8c>).

- The solver is now doing a better job when multiple versions of the
  same package are present in the same repository
  (<https://github.com/r-lib/actions/issues/559>).

- [`pkg_name_check()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_name_check.md)
  now works again, it needed a fix after changes at
  <https://crandb.r-pkg.org>.

- Explicit package names in local and URL package sources, as in
  `package=local::...` or `package=url::...` are now parsed correctly in
  dependencies.

- pkgdepends is now more robust to `Archs` fields missing from the CRAN
  metadata for packages with compiled code
  (<https://github.com/r-lib/pak/issues/448>).

- `url::` packages now always work correctly, even if the digest package
  is not installed (<https://github.com/r-lib/pak/issues/433>).

- pkgdepends is now more robust when installing packages from
  subdirectories of GitHub repositories
  (<https://github.com/r-lib/pak/issues/431>,
  [@paleolimbot](https://github.com/paleolimbot)).

- Parameters `?reinstall`,
  [`?source`](https://rdrr.io/r/base/source.html) and `?ignore` now work
  correctly when specified in the `package=?parameter` format
  ([\#294](https://github.com/r-lib/pkgdepends/issues/294)).

## pkgdepends 0.3.2

CRAN release: 2022-09-08

- The `?ignore` parameter works correctly now.

- dependency resolution now does not fail if a package is not found.

- pkgdepends can now install `url::` remotes from GitHub.

- pkgdepends now does not fail when the package of a `.tar.gz` GitHub
  snapshot is in a subdirectory, or in a subdirectory of a subdirectory.

- pkgdepends now errors early if it cannot deduce the name of the
  package from a `Remotes` or `Config/Needs/*` entry.

- Solver failures now include details in some cases where previously
  they did not.

- pkgdepends can now update packages in Docker containers where the old
  version was installed in the different Docker later
  (<https://github.com/r-lib/pak/issues/251>)

- pkgdepends errors are now user friendlier and better formatted.

## pkgdepends 0.3.1

CRAN release: 2022-04-08

- The dependency solver now uses better heuristics and does not
  (effectively) freeze if multiple repositories have multiple versions
  of the same packages (e.g. RSPM and CRAN)
  ([\#277](https://github.com/r-lib/pkgdepends/issues/277)).

## pkgdepends 0.3.0

CRAN release: 2022-02-16

- New `?ignore-before-r` parameter to ignore optional dependencies that
  need a newer R version
  ([\#243](https://github.com/r-lib/pkgdepends/issues/243)).

- New `?ignore` parameter to ignore an optional dependency.

- Allow specifying downstream package parameters with the
  `package=?param` syntax.

- The `$update()` operation now works better for `any::` refs, and we
  always install the version we planned for.

- System requirement installation is now more robust and works for Unix
  shell expressions
  ([\#347](https://github.com/r-lib/pkgdepends/issues/347)).

- Make system dependency installation more robust
  (<https://github.com/r-lib/pak/issues/347>).

- CRAN-like resolution is more robust now if a repository is missing the
  usual metadata.

- The lock file is pretty JSON now.

- pkgdepends now does not return tibbles, but simple data frames. They
  are still printed concisely as long as the pillar package is loaded.

- pkgdepends now handles all version requirement types properly: ‘\<’,
  ‘\<=’, `==`, `>=`, `>`.

## pkgdepends 0.2.0

CRAN release: 2021-11-29

- pkgdepends now has Much better platform support, including arm64
  packages on macOS, and single-arch and multi-arch binary packages on
  Windows.

- Better configuration via environment variables and options, see
  [`?"pkgdepends-config"`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
  for details.

- Many improvements for lock files, i.e. the `$create_lockfile()` method
  of `pkg_installation_proposal`.

- System requirements support. This is currently active on Linux, if the
  `CI` environment variable is set to `true`. You can set the
  `PKG_SYSREQS` environment variable to `true` to turn it on in other
  situations. See
  [`?"pkgdepends-config"`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md).

- The new `any::` reference type can be used to install a package from
  any source. See
  [`?pkg_refs`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for more about this.

## pkgdepends 0.1.2

CRAN release: 2021-06-30

- remotes can now update packages installed by pak. In the past this
  sometimes did not work
  ([\#301](https://github.com/r-lib/pkgdepends/issues/301)).

## pkgdepends 0.1.1

CRAN release: 2021-05-16

- [`pkg_name_check()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_name_check.md)
  works again.

## pkgdepends 0.1.0

CRAN release: 2021-03-16

First CRAN release.
