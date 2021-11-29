
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
