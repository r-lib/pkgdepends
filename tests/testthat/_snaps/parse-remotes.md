# parse_pkg_refs error on unknown type

    Code
      parse_pkg_refs(c("notgood::pkg", "good", "my_package"))
    Error <rlib_error_3_0>
      ! Cannot parse package: my_package.
      i See `?pkgdepends::pkg_refs()` for supported package sources.

