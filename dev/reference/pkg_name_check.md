# Check if an R package name is available.

Additionally, look up the candidate name in a number of dictionaries, to
make sure that it does not have a negative meaning.

## Usage

``` r
pkg_name_check(name, dictionaries = NULL)
```

## Arguments

- name:

  Package name candidate.

- dictionaries:

  Character vector, the dictionaries to query. Available dictionaries:
  \* `wikipedia` \* `wiktionary`, \* `sentiment`
  (<https://github.com/fnielsen/afinn>), \* `urban` (Urban Dictionary).
  If `NULL` (by default), the Urban Dictionary is omitted, as it is
  often offensive.

## Value

`pkg_name_check` object with a custom print method.

## Details

### Valid package name check

Check the validity of `name` as a package name. See 'Writing R
Extensions' for the allowed package names. Also checked against a list
of names that are known to cause problems.

### CRAN checks

Check `name` against the names of all past and current packages on CRAN,
including base and recommended packages.

### Bioconductor checks

Check `name` against all past and current Bioconductor packages.

### Profanity check

Check `name` with <https://www.purgomalum.com/service/containsprofanity>
to make sure it is not a profanity.

### Dictionaries

See the `dictionaries` argument.

## Examples

``` r
pkg_name_check("cli")
#> ╔═════════════════════════════════════════════════════════════════════╗
#> ║                             –*– cli –*–                             ║
#> ╚═════════════════════════════════════════════════════════════════════╝
#> ┌─────────────────────────────────────────────────────────────────────┐
#> │ ✔  valid name         ✖  CRAN                ✔  Bioconductor        │
#> │ ✔  not a profanity                                                  │
#> └─────────────────────────────────────────────────────────────────────┘
#> ┌ Wikipedia ──────────────────────────────────────────────────────────┐
#> │ CLI (from Cli) CLI may refer to multiple articles, see link.        │
#> └────────────────────────────────── https://en.wikipedia.org/wiki/CLI ┘
#> ┌ Wiktionary ─────────────────────────────────────────────────────────┐
#> │ cli No English definition found                                     │
#> └───────────────────────────────── https://en.wiktionary.org/wiki/cli ┘
#> ┌─────────────────────────────────────────────────────────────────────┐
#> │ Sentiment: :| (0)                                                   │
#> └─────────────────────────────────────────────────────────────────────┘
```
