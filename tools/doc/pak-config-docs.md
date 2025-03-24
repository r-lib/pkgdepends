$build_vignettes
[1] "Whether to build vignettes for package trees.\nThis is only used if the package is obtained from a package tree,\nand not from a source (or binary) package archive. By default\nvignettes are not built in this case. If you set this to \\code{TRUE},\nthen you need to make sure that the vignette builder packages are\navailable, as these are not installed by default currently."

$cache_dir
[1] "Directory to download the packages to. Defaults to a temporary\ndirectory within the R session temporary directory, see\n\\code{\\link[base:tempfile]{base::tempdir()}}."

$cran_mirror
[1] "CRAN mirror to use. Defaults to the \\code{repos} option\n(see \\code{\\link[base:options]{base::options()}}), if that's not set then\n\\verb{https://cran.rstudio.com}. See also \\code{\\link[pak:repo_add]{pak::repo_add()}} and\n\\code{\\link[pak:repo_get]{pak::repo_get()}}"

$git_submodules
[1] "Whether or not to update submodules in git repositories. This\naffects \\verb{git::} and \\verb{gitlab::} package sources only.\nIf the R package is in a subdirectory then only the submodules\nwithin that directory are updated. If a submodule appears in\n\\code{.Rbuildignore}, then it is skipped."

$include_linkingto
[1] "Whether to always include \\code{LinkingTo} dependencies in the solution\nof and installation, even if they are needed because the packages\nare installed from binaries. This is sometimes useful, see e.g.\n\\url{https://github.com/r-lib/pak/issues/485} for an example use case."

$library
[1] "Package library to install packages to. It is also used for\nalready installed packages when considering dependencies."

$metadata_cache_dir
[1] "Location of metadata replica of\n\\code{\\link[pkgcache:cranlike_metadata_cache]{pkgcache::cranlike_metadata_cache}}. Defaults to a temporary\ndirectory within the R session temporary directory, see\n\\code{\\link[base:tempfile]{base::tempdir()}}."

$metadata_update_after
[1] "A time interval as a \\link{difftime} object. pak will update the\nmetadata cache if it is older than this. The default is one day.\nThe \\code{PKG_METADATA_UPDATE_AFTER} environment variable may be set\nin seconds (\\code{s} suffix), minutes (\\code{m} suffix), hours (\\code{h} suffix),\nor days (\\code{d} suffix). E.g: \\verb{1d} means one day."

$package_cache_dir
[1] "Location of the package cache on the disk. See\n\\code{\\link[pak:cache]{pak::cache_summary()}}. Default is selected by pkgcache."

$platforms
[1] "Character vector of platforms to \\emph{download} or \\emph{install} packages\nfor. See \\code{\\link[pkgdepends:default_platforms]{pkgdepends::default_platforms()}} for possible platform\nnames. Defaults to the platform of the current R session, plus\n\\code{\"source\"}."

$r_versions
[1] "Character vector, R versions to download or install\npackages for. It defaults to the current R version."

$sysreqs
[1] "Whether to automatically look up and install system requirements.\nIf \\code{TRUE}, then pkgdepends will try to install required\nsystem packages. If \\code{FALSE}, then system requirements are still\nprinted (including OS packages on supported platforms), but they\nare not installed.\nBy default it is \\code{TRUE} on supported platforms,\nif the current user is the root user or password-less \\code{sudo} is\nconfigured for the current user."

$sysreqs_db_update
[1] "Whether to try to update the system requirements database from\nGitHub. If the update fails, then the cached or the build-in\ndatabase if used. Defaults to TRUE."

$sysreqs_db_update_timeout
[1] "Timeout for the system requirements database update.\nDefaults to five seconds, except if the \\code{CI} environment variable\nis set, then it is one minute."

$sysreqs_dry_run
[1] "If \\code{TRUE}, then pak only prints the system commands to\ninstall system requirements, but does not execute them."

$sysreqs_platform
[1] "The platform to use for system requirements lookup. On Linux, where\nsystem requirements are currently supported, it must be a string\ncontaining the distribution name and release, separated by a dash.\nE.g.: \\code{\"ubuntu-22.04\"}, or \\code{\"rhel-9\"}."

$sysreqs_rspm_repo_id
[1] "Posit Package Manager (formerly RStudio Package Manager) repository\nid to use for CRAN system requirements lookup. Defaults to the\n\\code{RSPM_REPO_ID} environment variable, if set. If not set, then it\ndefaults to \\code{1}."

$sysreqs_rspm_url
[1] "Root URL of Posit Package Manager (formerly RStudio Package\nManager) for system requirements lookup. By default the \\code{RSPM_ROOT}\nenvironment variable is used, if set. If not set,\nit defaults to \\verb{https://packagemanager.posit.co}."

$sysreqs_sudo
[1] "Whether to use \\code{sudo} to install system requirements,\non Unix. By default it is \\code{TRUE} on Linux if the effective user id\nof the current process is not the \\code{root} user."

$sysreqs_update
[1] "Whether to try to update system packages that are already installed.\nIt defaults to \\code{TRUE} on CI systems: if the \\code{CI} environment\nvariable is set to \\code{true}."

$sysreqs_verbose
[1] "Whether to echo the output of system requirements installation.\nDefaults to \\code{TRUE} if the \\code{CI} environment variable is set."

$use_bioconductor
[1] "Whether to automatically use the Bioconductor repositories.\nDefaults to \\code{TRUE}."

$windows_archs
[1] "Character scalar specifying which architectures\nto download/install for on Windows. Its possible values are:\n\\itemize{\n\\item \\code{\"prefer-x64\"}: Generally prefer x64 binaries. If the current R\nsession is \\code{x64}, then we download/install x64 packages.\n(These packages might still be multi-architecture binaries!)\nIf the current R session is \\code{i386}, then we download/install\npackages for both architectures. This might mean compiling\npackages from source if the binary packages are for \\code{x64} only,\nlike the CRAN Windows binaries for R 4.2.x currently.\n\\code{\"prefer-x64\"} is the default for R 4.2.0 and later.\n\\item \\code{\"both\"}: Always download/install packages for both \\code{i386} and\n\\code{x64} architectures. This might need compilation from source\nif the available binaries are for \\code{x64} only, like the CRAN\nWindows binaries for R 4.2.x currently. \\code{\"both\"} is the default\nfor R 4.2.0 and earlier.\n}"
