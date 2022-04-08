
# pkgdepends (development version)

No changes yet.

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
