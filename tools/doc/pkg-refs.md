\section{Package references}{
Many pkgdepends and pak functions take package names as arguments.
E.g. \code{pak::pkg_install()} takes the names of the packages to install and
\code{pak::pkg_deps_tree()} takes the names of the packages to draw dependency
trees for.

Most of these functions can also take more generic \emph{package references}
instead of package names.
A package reference also tells pak where to find the
package source.

To specify a package source, use its name as a prefix, with a \code{::}
separator.
E.g. \code{cran::mypkg} means the \code{mypkg} package from CRAN.

A package name is a special package reference that implicitly specifies
the configured CRAN(-like) repositories as the package source
(we call this the \code{standard} package source).
So \code{mypkg} is equivalent to \code{standard::mypkg} and pak
looks for mypkg in any of the configured CRAN-like repositories.
If you did not explicitly specify any CRAN-like repositories (e.g. with
\code{options("repos")}), then pak uses the CRAN and
Bioconductor repositories by default.

This is the list of the currently supported package sources.
We will discuss each in detail below.
\itemize{
\item \code{cran}: a CRAN package.
\item \code{bioc}: a Bioconductor package.
\item \code{standard}: a package from a configured CRAN-like repository.
\item \code{github}: a package from GitHub.
\item \code{gitlab}: a package from GitLab.
\item \code{git}: a package in a Git repository.
\item \code{local}: a local package file or directory.
\item \code{url}: an URL that points to a package archive.
\item \code{installed}: an installed package.
\item \code{deps}: the dependencies of a local package file or directory.
\item \code{any}: a special reference type that accepts a package from any source.
See below.
\item \code{param}: a special reference to change how other references are downloaded
or installed. See "Parameters" below.
}
\subsection{Shorthands}{

To save typing, you do not always need to fully specify the package source
in a package reference.
You have seen before that a package name implicitly has a \code{standard}
package source.
Here are the complete rules for such shorthands, in the order they are
applied:

If the package reference is
\itemize{
\item a valid package name, or a package name with an \code{@}
version specification, the \code{standard} package source is used. E.g. \code{pkg} is
equivalent to \code{standard::pkg} and \verb{pkg@1.0} is equivalent to
\verb{standard::pkg@1.0}.
\item a valid \code{github} ref type without the \verb{github::} prefix,
then \code{github} is used. E.g. \code{user/repo} is equivalent to
\code{github::user/repo} and \code{user/repo@tag} is equivalent to
\code{github::user/repo@tag}, etc.
\item a GitHub URL (see below) without the \verb{github::} prefix,
then \code{github} is used.
\item a path that starts with \code{.} or \code{/} or \verb{\\} or \code{~}, then
\code{local} is used. (pak does not check if the path
exists.)
\item of the form \verb{<package-name>=?<parameters>},
then it will be the special \code{param} type. See "Parameters" below.
}

If the package reference does not have an explicit package source,
and the package source cannot be determined from these rules, then
pak throws an error.
}

\subsection{Package names}{

When pak is looking up the dependencies of a package,
it needs to be able to determine the name of the dependency from the
package reference.
This is sometimes not easy for dependencies in \code{Remotes} (or similar) fields.
\itemize{
\item For \verb{github::} and \verb{gitlab::} dependencies pak assumes
that the package name is the same as the name of the repository. If this
does not hold, then you need to specify the package name explicitly, using
a \verb{<package>=} prefix. E.g. \code{pins=rstudio/pins-r}. To specify both the
package source type and the package name at the same time, write it like this:
\code{pins=github::rstudio/pins-r}.
\item For \verb{git::} dependencies, pak assumes that the package
name is the same as the last component of the repository. If this does not
hold, then you need to specify the package name explicitly, using a
\verb{<package>=} prefix. E.g. \verb{pins=git::https://github.com/rstudio/pins-r}.
\item For \verb{local::} dependencies, you always need to specify the package name
explicitly. E.g. \verb{pins=local::~/works/pins}.
\item For \verb{url::} dependencies, you always need to specify the package name
explicitly. E.g. \verb{ggplot2=url::https://cloud.r-project.org/src/contrib/...}.
}
}

\subsection{Parameters}{

Package references may have optional parameters, added after a question
mark.
Different parameters are separated by an ampersand (\code{&}) character.
(This is very similar to how HTTP URLs take query parameters.)

Parameters may be flags that turn on some behavior, or they can have a
string value, assigned with an equal sign (\code{=}).
If no value is assigned, then a \code{true} value is assumed. For example,
these two package references are equivalent:

\if{html}{\out{<div class="sourceCode">}}\preformatted{cran::testthat?source&nocache
cran::testthat?source=true&nocache=true
}\if{html}{\out{</div>}}
\subsection{Parameters for downstream packages}{

pak allows specifying parameters for downstream packages,
using the \verb{<package>=?<params>} special package reference, where \code{package} is
the name of the package, and \verb{<params>} are the parameters, as above.
This is useful if you want to add a parameter to a downstream dependency.

For example, to install ggplot2, and always reinstall its cli package
dependency, you could use the \code{ggplot2} and \code{cli=?reinstall} package
references.
The latter tells pak to always reinstall cli, even if
it is already installed.
}

\subsection{Currently supported parameters}{
\itemize{
\item \code{ignore} is a flag parameter. If specified, the package is ignored.
This usually makes sense in the \code{packagename=?ignore} form, to ignore a
downstream soft dependency. If all versions of a hard dependency are
ignored that will lead to a solution error.
\item \code{ignore-before-r} is a version number parameter. The package will be
ignored on R versions that are older than the specified one. E.g.
\verb{Matrix=?ignore-before-r=4.1.2} will ignore the Matrix package on R versions
that are older than 4.1.2. This parameter really only makes sense in the
\code{packgename=?ignore-before-r} form.
\item \code{ignore-unavailable} is a flag. It can only be specified for soft
dependencies. If specified and the package is not available, it will be
ignored. This parameter really only makes sense in the
\code{packagename=?ignore-unavailable} form.
\item \code{source} is a flag parameter. If specified, then a source R package
is requested from a CRAN-like repository. For package installations,
\code{source} always triggers a re-install. In other words, \code{source} implies the
\code{reinstall} parameter. This parameter is supported for \verb{bioc::}, \verb{cran::}
and \verb{standard::} remote types, and it is ignored for others.
\item \code{reinstall} requests a re-install for package installations. It is
supported by the \verb{bioc::}, \verb{cran::}, \verb{git::}, \verb{github::}, \verb{gitlab::},
\verb{local::}, \verb{standard::}, and \verb{url::} remote types.
\item \code{nocache} will ignore the package cache. It will always download the
package file, and it will not add the downloaded (and built) package(s)
to the package cache. It is supported by the \verb{bioc::}, \verb{cran::}, \verb{git::},
\verb{github::}, \verb{gitlab::}, \verb{standard::} and \verb{url::} remote types.
}
}

}

\subsection{Package source details}{
\subsection{CRAN packages (\verb{cran::})}{

A package from CRAN. Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{[cran::]<package>[@[>=]<version> | @current | @last]
}\if{html}{\out{</div>}}
\itemize{
\item \verb{<package>} is a valid package name.
\item \verb{<version>} is a version or a version requirement.
}

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{forecast
forecast@8.8
forecast@>=8.8
cran::forecast
forecast@last
forecast@current
}\if{html}{\out{</div>}}

Note: pak currently parses the version specification part
(everything after \code{@}), but does not use it.
}

\subsection{Bioconductor packages (\verb{bioc::})}{

A package from Bioconductor. The syntax is the same as for CRAN packages,
except for the prefix.

\if{html}{\out{<div class="sourceCode">}}\preformatted{[bioc::]<package>[@[>=]<version> | @current | @last]
}\if{html}{\out{</div>}}
}

\subsection{Standard packages (\verb{standard::})}{

These are packages either from CRAN or Bioconductor, the full syntax
is the same as for CRAN packages, except for the prefix:

\if{html}{\out{<div class="sourceCode">}}\preformatted{[standard::]<package>[@[>=]<version> | current | last]
}\if{html}{\out{</div>}}
}

\subsection{GitHub packages (\verb{github::})}{

Packages from a GitHub repository. Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{[<package>=][github::]<username>/<repository>[/<subdir>][<detail>]
}\if{html}{\out{</div>}}
\itemize{
\item \verb{<package>} is the name of the package. If this is missing, then
the name of the repository is used.
\item \verb{<username>} is a GitHub username or organization name.
\item \verb{<repository>} is the name of the repository.
\item \verb{<subdir>} specifies an optional subdirectory, if the package is within a
subdirectory in the repository.
\item \verb{<detail>} specifies a certain version of the package, see below.
}

\verb{<detail>} may specify:
\itemize{
\item a Git branch, tag or (prefix of) a commit hash: \verb{@<commitish>};
\item a pull request: \verb{#<pull-request>}; or
\item the latest release: \verb{@*release}.
}

If \verb{<detail>} is missing, then the latest commit of the \emph{default}
branch is used.

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{r-lib/crayon
github::r-lib/crayon
r-lib/crayon@84be6207
r-lib/crayon@branch
r-lib/crayon#41
r-lib/crayon@release
}\if{html}{\out{</div>}}

For convenience, GitHub HTTP URLs can also be used to specify a
package from GitHub. Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{https://github.com/r-lib/withr
# A branch:
https://github.com/r-lib/withr/tree/ghactions
# A tag:
https://github.com/r-lib/withr/tree/v2.1.1
# A commit:
https://github.com/r-lib/withr/commit/8fbcb548e316
# A pull request:
https://github.com/r-lib/withr/pull/76
# A release:
https://github.com/r-lib/withr/releases/tag/v2.1.0
}\if{html}{\out{</div>}}

A GitHub remote string can also be used instead of a URL, for example:
\code{git@github.com:r-lib/pak.git}
}

\subsection{GitLab packages (\verb{gitlab::})}{

Packages from a GitLab repository. Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{[<package>=][gitlab::]<project-path>/<repository>[/-/<subdir>][<detail>]
}\if{html}{\out{</div>}}
\itemize{
\item \verb{<package>} is the name of the package. If this is missing, then
the name of the repository is used.
\item \verb{<project-path>} is typically the GitLab user or group name, but
it may contain subgroups.
\item \verb{<repository>} is the name of the repository, or the project in GitLab
terminology. GitLab
\href{https://docs.gitlab.com/ee/user/group/subgroups/}{subgroups} are
fully supported.
\item \verb{<subdir>} specifies an optional subdirectory, if the package is within a
subdirectory in the repository. Note that for GitLab, this must come
after a \verb{/-} prefix, to be able to distinguish it from subgroups.
\item \verb{<detail>} may specify a Git branch, tag or (prefix of) a commit hash.
}

If \verb{<detail>} is missing, then the latest commit of the \emph{default}
branch is used.

\verb{gitlab::} supports Git submodules, see the \code{git-submodules} configuration
entry.

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{gitlab::gaborcsardi/cli
gitlab::r-hub/filelock@main
gitlab::group/subgroup/subsubgroup/project/-/subdir@ref
}\if{html}{\out{</div>}}
}

\subsection{Packages in Git repositories (\verb{git::})}{

Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{[<package>=]git::https?://<host>[<detail>]
}\if{html}{\out{</div>}}
\itemize{
\item \verb{<package>} is the name of the package. If this is missing, then
the last component of the \verb{<host>} is used.
\item \verb{<host>} is the host name and path of the Git repository. Some Git
repositories need the \code{.git} suffix here, others are more forgiving.
\item \verb{<detail>} specifies a certain version of the package:
a Git branch, tag or (prefix of) a commit hash: \verb{@<commitish>}.
}

If \verb{<detail>} is missing, then the latest commit of the \emph{default}
branch is used.

\verb{git::} supports Git submodules, see the \code{git-submodules} configuration
entry.

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{git::https://github.com/r-lib/crayon
git::https://github.com/r-lib/crayon.git
git::https://github.com/r-lib/crayon.git@84be6207
git::https://github.com/r-lib/crayon.git@branch
git::https://gitlab.com/gaborcsardi/cli.git
}\if{html}{\out{</div>}}

Note that pak has a built-in Git client, and does
\strong{not} require a system Git installation.

If the system has Git installed, then pak will use the
credentials stored in the configured Git credential store, automatically,
via the gitcreds package.
}

\subsection{Local packages (\verb{local::})}{

A path that refers to a package file built with \verb{R CMD build}, or a
directory that contains a package. Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{local::<path>
}\if{html}{\out{</div>}}

For brevity, you can omit the \verb{local::} prefix, if you specify an
absolute path, a path from the user's home directory, starting with \code{~},
or a relative path starting with \verb{./} or \verb{.\\\\}.

A single dot (\code{"."}) is considered to be a local package in the current
working directory.

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{local::/foo/bar/package_1.0.0.tar.gz
local::/foo/bar/pkg
local::.
/absolute/path/package_1.0.0.tar.gz
~/path/from/home
./relative/path
.
}\if{html}{\out{</div>}}

If you specify a local package in a dependency (i.e. in \code{DESCRIPTION}), then
you also need to specify the name of the package, see "Package names"
above.
}

\subsection{URLs (\verb{url::})}{

You can use \verb{url::} to refer to URLs that hold R package archives
(i.e. properly built with \verb{R CMD build}), or compressed directories
of package trees (i.e. not built with \verb{R CMD build}). pak will
figure out if it needs to run \verb{R CMD build} on the package first.

This remote type supports \code{.tar.gz} and \code{.zip} files.

Note that URLs are not ideal remote types, because pak needs to
download the package file to resolve its dependencies. When this happens,
it puts the package file in the cache, so no further downloads are
needed when installing the package later.

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{url::https://cloud.r-project.org/src/contrib/Archive/cli/cli_1.0.0.tar.gz
url::https://github.com/tidyverse/stringr/archive/HEAD.zip
}\if{html}{\out{</div>}}

If you specify a package from a URL in a dependency (i.e. in \code{DESCRIPTION}),
then you also need to specify the name of the package, see "Package names"
above.
}

\subsection{Installed packages (\verb{installed::})}{

This is usually used internally, but can also be used directly.
Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{installed::<path>/<package>
}\if{html}{\out{</div>}}
\itemize{
\item \verb{<path>} is the library the package is installed to.
\item \verb{<package>} is the package name.
}

Example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{installed::~/R/3.6/crayon
}\if{html}{\out{</div>}}
}

\subsection{Package dependencies (\verb{deps::})}{

Usually used internally, it specifies the dependencies of a local
package. It can be used to download or install the dependencies of a
package, without downloading or installing the package itself.
Full syntax:

\if{html}{\out{<div class="sourceCode">}}\preformatted{deps::<path>
}\if{html}{\out{</div>}}

Examples:

\if{html}{\out{<div class="sourceCode">}}\preformatted{deps::/foo/bar/package_1.0.0.tar.gz
deps::/foo/bar/pkg
deps::.
}\if{html}{\out{</div>}}
}

\subsection{\verb{any::} packages}{

Sometimes you need to install additional packages, but you don't mind
where they are installed from. Here is an example. You want to install
cli from GitHub, from \code{r-lib/cli}. You also want to install glue, and
you don't mind which version of glue is installed, as long as it is
compatible with the requested cli version. If cli specifies the
development version of glue, then that is fine. If cli is fine with the
CRAN version of glue, that's OK, too. If a future version of cli does
not depend on glue, you still want glue installed, from CRAN. The \verb{any::}
reference type does exactly this.

In our example you might write

\if{html}{\out{<div class="sourceCode r">}}\preformatted{pak::pkg_install(c("glue", "r-lib/cli"))
}\if{html}{\out{</div>}}

first, but this will fail if \code{rlib/cli} requests (say) \code{tidyverse/glue},
because in \code{pkg_install()} \code{"glue"} is interpreted as \code{"standard::glue"},
creating a conflict with \code{tidyverse/glue}. On the other hand

\if{html}{\out{<div class="sourceCode r">}}\preformatted{pak::pkg_install(c("any::glue", "r-lib/cli"))
}\if{html}{\out{</div>}}

works, independently of which glue version is requested by cli.
}

\subsection{Parameter refs (\verb{param::})}{

See "Parameters" above.
}

}

\subsection{The \code{Remotes} field}{

In the \code{DESCRIPTION} file of an R package you can mark any regular
dependency defined in the \code{Depends}, \code{Imports}, \code{Suggests} or \code{Enhances}
fields as being installed from a non-standard package source by adding
a package reference to a \code{Remotes} entry.
pak will download and install the package from the
specified location, instead of a CRAN-like repository.

The remote dependencies specified in \code{Remotes} are a comma separated
list of package sources:

\if{html}{\out{<div class="sourceCode">}}\preformatted{Remotes: <pkg-source-1>, <pkg-source-2>, [ ... ]
}\if{html}{\out{</div>}}

Note that you will still need to add the package to one of the regular
dependency fields, i.e. \code{Imports}, \code{Suggests}, etc. Here is a concrete
example that specifies the \code{r-lib/glue} package:

\if{html}{\out{<div class="sourceCode">}}\preformatted{Imports: glue
Remotes: r-lib/glue,
  r-lib/httr@v0.4,
  klutometis/roxygen#142,
  r-lib/testthat@c67018fa4970
}\if{html}{\out{</div>}}

The CRAN and Bioconductor repositories do not support the \code{Remotes}
field, so you need to remove this field, before submitting your package
to either of them.
}
}
