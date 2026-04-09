# Query and manipulate CRAN-like repositories

These functions are currently experimental.

## Details

### List packages in a repository

`repo$list()` lists packages in a repository. It reads the `PACKAGES`
file containing the repository metadata.

#### Usage

    repo_list(..., path = ".")

#### Arguments

- `...`: ignored currently.

- `path`: path to repository. Must contain a `PACKAGES` file.

#### Value

Data frame of package data, a data frame with at least the following
columns, possibly more if there are other entries in the metadata:

- `Package`

- `Version`

- `Depends`

- `Imports`

- `Suggests`

- `Enhances`

- `LinkingTo`

- `License`

- `File`

- `DownloadURL`

- `OS`

- `Arch`

- `Built`

- `Filesize`

- `SHA256`

- `RVersion`

- `Platform`

- `GraphicsAPIVersion`

- `InternalsId`

- `SystemRequirements`

### Delete packages from repository metadata

`repo$delete()` deletes matching packages from the repository metadata.

#### Description

All matching packages will be removed. It does not delete the files
themselves.

#### Usage

    repo$delete(package, ..., path = ".")

#### Arguments

- `package`: package name.

- `...`: other fields to match, they must be named. Matching is case
  insensitive.

- `path`: path to repository. Must contain a `PACKAGES` file.

### Add a package to a repository

#### Description

It does not check if any version of the package is already in the
repository. If you want to *update* a package, use `repo$update()`.

#### Usage

    repo$add(file, ..., path = ".")

#### Arguments

- \`file Package file.

- \`... Ignored currently.

- `path Path to repository. Must contain a `PACKAGES\` file.

### Update a package in a repository

#### Description

Previous version of the same package are removed. In particular, it
removes packages with matching:

- package name (`Package` field),

- R version (`Rversion` field),

- same OS (`OS` field), or no `OS` field,

- same architecture (`Arch` field), or not `Arch` field.

#### Usage

    repo$update(file, ..., path = ".")

#### Arguments

- \`file Package file.

- \`... Ignored currently.

- `path Path to repository. Must contain a `PACKAGES\` file.

### Update a file in a package metadata, stored on GitHub

#### Description

1.  Clones the GitHub repository.

2.  Calls `repo_update()` with `file`, in the `subdir` directory.

3.  Adds and commits changes.

4.  Pushes the git repository to GitHub. If the push fails, then it
    resets the git repository, pulls it from GitHub and tries the update
    process again, until the push succeeds.

This function needs command line git installed.

It sets up a `cache` credential helper, so the `git push` works without
interaction with the user.

#### Usage

    repo$update_gh(repo, subdir, files)

#### Arguments

- `repo`: GitHub slug, e.g. `r-hub/repos`.

- `subdir`: subdirectory in the GitHub repository, where the R package
  metadata should be updated. It must exist in the repository. If it
  does not have `PACKAGES*` files, then they will be created.

- `files`: package files to add. The files will *not* be added to the
  repository, only to the metadata. They should be in the repository
  already.
