# Parse package location references

See
[pkg_refs](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
for more about supported package references.

## Usage

``` r
parse_pkg_refs(refs, remote_types = NULL, ...)

parse_pkg_ref(ref, remote_types = NULL, ...)
```

## Arguments

- refs:

  Character vector of references.

- remote_types:

  Custom remote types can be added here, this is for advanced use, and
  experimental currently.

- ...:

  Additional arguments are passed to the individual parser functions.

- ref:

  A package reference, like `refs`, but a length one vector, for
  convenience.

## Value

`parse_pkg_refs()` returns a list of parsed references.
`parse_pkg_ref()` returns one parsed reference. A parsed reference is a
list, with at least elements:

- `ref`: The original reference string.

- `type`: The reference type.

- `package`: The package name. It typically contains additional data,
  specific to the various reference types. See
  [pkg_refs](https://r-lib.github.io/pkgdepends/dev/reference/pkg_refs.md)
  for details. The parsed reference always has class `remote_ref_<type>`
  and `remote_ref`.
