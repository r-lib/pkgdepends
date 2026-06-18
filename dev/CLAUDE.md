# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

Load the package:

``` r

uncovr::reload()
```

Run the test suite:

``` r

uncovr::test()
```

Run a single test file:

``` r

uncovr::test(filter = "solve")
```

Regenerate documentation (man/ files):

``` r

uncovr::document()
```

Run full package check:

``` r

withr::with_envvar(c(NOT_CRAN = "true", DISPLAY = NA), rcmdcheck::rcmdcheck())
```

## Architecture

pkgdepends is the dependency-resolution, download and installation
engine behind [pak](https://github.com/r-lib/pak). It is a *toolkit*
meant to be used as a dependency, not an end-user package manager. It
handles packages from CRAN, Bioconductor and other CRAN-like repos,
GitHub, GitLab, git repositories, URLs, and local trees/files. Metadata
and package files are cached via the
[pkgcache](https://github.com/r-lib/pkgcache) package, and all HTTP
requests, downloads, builds and installations run in parallel.

**The pipeline.** Everything flows through four stages, in order:

1.  **Resolve** — expand a set of package references into every
    candidate package (recursively following dependencies), querying
    each source for available versions and metadata.
2.  **Solve** — pick a single consistent set of versions that can be
    installed together, using an integer-program solver (`lpSolve`).
3.  **Download** — fetch the chosen package files (from cache when
    possible).
4.  **Install** — build and install the downloaded files in dependency
    order, in parallel.

**Public R6 classes** (each is a façade over the internal `pkg_plan`
engine, exposing a subset of the pipeline):

| Class | Constructor | Stages exposed |
|----|----|----|
| `pkg_deps` | [`new_pkg_deps()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_deps.md) | resolve + solve (dependency queries only) |
| `pkg_download_proposal` | [`new_pkg_download_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_download_proposal.md) | resolve + solve + download |
| `pkg_installation_proposal` | [`new_pkg_installation_proposal()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_proposal.md) | full pipeline |
| `pkg_installation_plan` | [`new_pkg_installation_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_installation_plan.md) | install from an existing lockfile |

**Package references.** A *ref* is a string like `pkg`,
`github::user/repo`, `bioc::pkg`, `url::https://...`, `local::./path`,
`installed::...`. Each ref *type* has its own file: see `R/type-*.R`
(e.g. `type-cran.R`, `type-github.R`, `type-gitlab.R`, `type-git.R`,
`type-url.R`, `type-local.R`, `type-installed.R`). `R/parse-remotes.R`
parses ref strings.

**Key source files:**

| File | Role |
|----|----|
| [R/pkg-plan.R](https://r-lib.github.io/pkgdepends/dev/R/pkg-plan.R) | `pkg_plan` R6 class — the internal engine that drives all four stages; the public classes delegate to it |
| [R/pkg-dependencies.R](https://r-lib.github.io/pkgdepends/dev/R/pkg-dependencies.R) | `pkg_deps` public class |
| [R/pkg-downloads.R](https://r-lib.github.io/pkgdepends/dev/R/pkg-downloads.R) | `pkg_download_proposal` public class |
| [R/pkg-installation.R](https://r-lib.github.io/pkgdepends/dev/R/pkg-installation.R) | `pkg_installation_proposal` / `pkg_installation_plan` public classes |
| [R/resolution.R](https://r-lib.github.io/pkgdepends/dev/R/resolution.R) | Resolution stage — drives ref-type resolvers, builds the resolution data frame |
| [R/resolution-df.R](https://r-lib.github.io/pkgdepends/dev/R/resolution-df.R) | Schema/manipulation of the resolution data frame |
| [R/solve.R](https://r-lib.github.io/pkgdepends/dev/R/solve.R) | Dependency solver — formulates and runs the `lpSolve` integer program |
| [R/download.R](https://r-lib.github.io/pkgdepends/dev/R/download.R) | Download stage |
| [R/install-plan.R](https://r-lib.github.io/pkgdepends/dev/R/install-plan.R) | Turns a solved plan into an ordered install plan; [`install_package_plan()`](https://r-lib.github.io/pkgdepends/dev/reference/install_package_plan.md) |
| [R/install-tar.R](https://r-lib.github.io/pkgdepends/dev/R/install-tar.R) / [R/install-zip.R](https://r-lib.github.io/pkgdepends/dev/R/install-zip.R) / [R/install-binary.R](https://r-lib.github.io/pkgdepends/dev/R/install-binary.R) | Low-level installers |
| [R/build.R](https://r-lib.github.io/pkgdepends/dev/R/build.R) / [R/builder.R](https://r-lib.github.io/pkgdepends/dev/R/builder.R) | Building packages from source |
| [R/parse-remotes.R](https://r-lib.github.io/pkgdepends/dev/R/parse-remotes.R) | Parse package reference strings |
| [R/config.R](https://r-lib.github.io/pkgdepends/dev/R/config.R) / [R/zzz-pkgdepends-config.R](https://r-lib.github.io/pkgdepends/dev/R/zzz-pkgdepends-config.R) | Configuration system and defaults |
| [R/sysreqs2.R](https://r-lib.github.io/pkgdepends/dev/R/sysreqs2.R) / [R/sysreqs.R](https://r-lib.github.io/pkgdepends/dev/R/sysreqs.R) | System requirements detection and install plans |
| [R/scan-deps.R](https://r-lib.github.io/pkgdepends/dev/R/scan-deps.R) | Scan source files for dependencies (uses tree-sitter, see [R/tree-sitter.R](https://r-lib.github.io/pkgdepends/dev/R/tree-sitter.R)) |
| [R/git-protocol.R](https://r-lib.github.io/pkgdepends/dev/R/git-protocol.R) / [R/git-auth.R](https://r-lib.github.io/pkgdepends/dev/R/git-auth.R) | Pure-R git protocol client and auth, for git/GitHub/GitLab refs |
| [R/errors.R](https://r-lib.github.io/pkgdepends/dev/R/errors.R) / [R/pkgdepends-errors.R](https://r-lib.github.io/pkgdepends/dev/R/pkgdepends-errors.R) | Error classes and condition handling |

**Concurrency.** Resolution, downloads and HTTP queries are asynchronous
— most engine methods have an `async_*` variant (e.g. `async_resolve()`)
returning a deferred value; the synchronous methods wrap them in an
event loop. Installation runs subprocesses in parallel via
processx/callr.

## Testing

Tests live in `tests/testthat/`. Snapshot files are in
`tests/testthat/_snaps/`. Fixtures (sample `PACKAGES` files, test
package trees, captured command output) live in
`tests/testthat/fixtures/` and `tests/testthat/_fixtures/`. Several
`helper-*.R` files set up fake HTTP servers (via `webfakes`), git apps,
and resolution/solve fixtures.

Many tests are skipped on CRAN (`skip_on_cran()`). The `NOT_CRAN`
environment variable controls this during local development. Tests that
hit the network or spin up fake servers rely on the helper apps in
`helper-apps.R`.
