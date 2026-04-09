# Update a CRAN-like repository of binary packages at GitHub

These functions are currently experimental.

## Details

### Update a CRAN-like repository of binary packages at GitHub

`ghrepo$update()` updates a binary package mirror.

#### Usage

    ghrepo$update(
      repo,
      subdir,
      release_org = "cran",
      source_repo = "https://cran.r-project.org",
      packages = NULL
    )

#### Arguments

- `repo`: GitHub slug, e.g. `r-hub/repos`.

- `subdir`: subdirectory in the GitHub repository, where the R package
  metadata should be updated. It must exist in the repository. If it
  does not have `PACKAGES*` files, then they will be created.

- `release_org`: GitHub organization or user name where the packages
  will be published as releases.

- `source_repo`: A CRAN-like repository, where source packages are taken
  from.

- `packages`: A character vector of package names to add to the binary
  repository, in addition to updating the ones that are already there.
