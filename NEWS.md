# pkgdepends 0.5.0

* pkgdepends now support git repositories as package references. E.g.
  `git::https://github.com/r-lib/pak.git`.

* pkgdepends now supports versioned CRAN packages, e.g. `dplyr@1.1.1` will
  always install dplyr 1.1.1. Note that only CRAN packages are supported,
  Bioconductor packages are not (yet).

* pkgdepends now has an alternative system requirements lookup
  implementation. It supports Fedora and Debian systems as well, in
  addition to Debian, Ubuntu, SUSE and RedHat derivatives.
  You can switch to this implementation by setting the
  `R_PKG_SYSREQS2` environment variable to `true`.

* pkgdepends now does a better job looking up dependencies for
  hand-selected dependency types. E.g. `dependencies = "LinkingTo"`.

* pkgdepends now removes `?ignore`-d packages from dependencies, and
  uses the correct version comparison for `?ignore-before.r`
  (https://github.com/r-lib/actions/issues/708).

* pkgdepends now does not fail for circular soft dependencies (#306).

* pkgdepends now reports dependency solver failures better in some cases
  (#305, https://github.com/r-lib/pak/issues/474).

* pkgdepends now uses locally built CRAN binaries from the cache.

# pkgdepends 0.4.0

* pkgdepends has much improved and more informative error messages now.
  This work is not yet finished, so if you find an unclear error message,
  please open an issue. Thank you!

* The solver is now more robust for non-canonical input (e.g. `DESCRIPTION`
  files) (https://github.com/r-lib/pak/issues/423).

* Better installation output. Standard output and error are now
  collected together (https://github.com/r-lib/pkgdepends/commit/0669f0f8c).

* The solver is now doing a better job when multiple versions of the
  same package are present in the same repository
  (https://github.com/r-lib/actions/issues/559).

* `pkg_name_check()` now works again, it needed a fix after changes at
  https://crandb.r-pkg.org.

* Explicit package names in local and URL package sources, as in
  `package=local::...` or `package=url::...` are now parsed correctly in
  dependencies.

* pkgdepends is now more robust to `Archs` fields missing from the CRAN
  metadata for packages with compiled code
  (https://github.com/r-lib/pak/issues/448).

* `url::` packages now always work correctly, even if the digest package is
  not installed (https://github.com/r-lib/pak/issues/433).

* pkgdepends is now more robust when installing packages from subdirectories
  of GitHub repositories (https://github.com/r-lib/pak/issues/431,
  @paleolimbot).

* Parameters `?reinstall`, `?source` and `?ignore` now work correctly when
  specified in the `package=?parameter` format (#294).

# pkgdepends 0.3.2

* The `?ignore` parameter works correctly now.

* dependency resolution now does not fail if a package is not found.

* pkgdepends can now install `url::` remotes from GitHub.

* pkgdepends now does not fail when the package of a `.tar.gz` GitHub
  snapshot is in a subdirectory, or in a subdirectory of a subdirectory.

* pkgdepends now errors early if it cannot deduce the name of the package
  from a `Remotes` or `Config/Needs/*` entry.

* Solver failures now include details in some cases where previously they
  did not.

* pkgdepends can now update packages in Docker containers where the
  old version was installed in the different Docker later
  (https://github.com/r-lib/pak/issues/251)

* pkgdepends errors are now user friendlier and better formatted.

# pkgdepends 0.3.1

* The dependency solver now uses better heuristics and does not
  (effectively) freeze if multiple repositories have multiple versions of
  the same packages (e.g. RSPM and CRAN) (#277).

# pkgdepends 0.3.0

* New `?ignore-before-r` parameter to ignore optional dependencies that
  need a newer R version (#243).

* New `?ignore` parameter to ignore an optional dependency.

* Allow specifying downstream package parameters with the `package=?param`
  syntax.

* The `$update()` operation now works better for `any::` refs, and we
  always install the version we planned for.

* System requirement installation is now more robust and works for
  Unix shell expressions (#347).

* Make system dependency installation more robust
  (https://github.com/r-lib/pak/issues/347).

* CRAN-like resolution is more robust now if a repository is missing
  the usual metadata.

* The lock file is pretty JSON now.

* pkgdepends now does not return tibbles, but simple data frames.
  They are still printed concisely as long as the pillar package is loaded.

* pkgdepends now handles all version requirement types properly:
  '<', '<=', `==`, `>=`, `>`.

# pkgdepends 0.2.0

* pkgdepends now has Much better platform support, including arm64 packages
  on macOS, and single-arch and multi-arch binary packages on Windows.

* Better configuration via environment variables and options, see
  `?"pkgdepends-config"` for details.

* Many improvements for lock files, i.e. the `$create_lockfile()` method
  of `pkg_installation_proposal`.

* System requirements support. This is currently active on Linux, if the
  `CI` environment variable is set to `true`. You can set the
  `PKG_SYSREQS` environment variable to `true` to turn it on in other
  situations. See `?"pkgdepends-config"`.

* The new `any::` reference type can be used to install a package from
  any source. See `?pkg_refs` for more about this.

# pkgdepends 0.1.2

* remotes can now update packages installed by pak. In the past this
  sometimes did not work (#301).

# pkgdepends 0.1.1

* `pkg_name_check()` works again.

# pkgdepends 0.1.0

First CRAN release.
