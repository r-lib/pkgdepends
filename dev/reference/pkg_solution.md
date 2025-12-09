# The dependency solver

The dependency solver takes the resolution information, and works out
the exact versions of each package that must be installed, such that
version and other requirements are satisfied.

## Details

### Solution policies

The dependency solver currently supports two policies: `lazy` and
`upgrade`. The `lazy` policy prefers to minimize installation time, and
it does not perform package upgrades, unless version requirements
require them. The `upgrade` policy prefers to update all package to
their latest possible versions, but it still considers that version
requirements.

### The integer problem

Solving the package dependencies requires solving an integer linear
problem (ILP). This subsection briefly describes how the problem is
represented as an integer problem, and what the solution policies
exactly mean.

Every row of the package resolution is a candidate for the dependency
solver. In the integer problem, every candidate corresponds to a binary
variable. This is 1 if that candidate is selected as part of the
solution, and 0 otherwise.

The objective of the ILP minimization is defined differently for
different solution policies. The ILP conditions are the same.

1.  For the `lazy` policy, `installed::` packaged get 0 points, binary
    packages 1 point, sources packages 5 points.

2.  For the 'upgrade' policy, we rank all candidates for a given package
    according to their version numbers, and assign more points to older
    versions. Points are assigned by 100 and candidates with equal
    versions get equal points. We still prefer installed packages to
    binaries to source packages, so also add 0 point for already
    installed candidates, 1 extra points for binaries and 5 points for
    source packages.

3.  For directly specified refs, we aim to install each package exactly
    once. So for these we require that the variables corresponding to
    the same package sum up to 1.

4.  For non-direct refs (i.e. dependencies), we require that the
    variables corresponding to the same package sum up to at most one.
    Since every candidate has at least 1 point in the objective function
    of the minimization problem, non-needed dependencies will be
    omitted.

5.  For direct refs, we require that their candidates satisfy their
    references. What this means exactly depends on the ref types. E.g.
    for CRAN packages, it means that a CRAN candidate must be selected.
    For a standard ref, a GitHub candidate is OK as well.

6.  We rule out candidates for which the dependency resolution failed.

7.  We go over all the dependency requirements and rule out packages
    that do not meet them. For every package `A`, that requires package
    `B`, we select the `B(i, i=1..k)` candidates of `B` that satisfy
    `A`'s requirements and add a `A - B(1) - ... - B(k) <= 0` rule. To
    satisfy this rule, either we cannot install `A`, or if `A` is
    installed, then one of the good `B` candidates must be installed as
    well.

8.  We rule out non-installed CRAN and Bioconductor candidates for
    packages that have an already installed candidate with the same
    exact version.

9.  We also rule out source CRAN and Bioconductor candidates for
    packages that have a binary candidate with the same exact version.

### Explaining why the solver failed

To be able to explain why a solution attempt failed, we also add a dummy
variable for each directly required package. This dummy variable has a
very large objective value, and it is only selected if there is no way
to install the directly required package.

After a failed solution, we look the dummy variables that were selected,
to see which directly required package failed to solve. Then we check
which rule(s) ruled out the installation of these packages, and their
dependencies, recursively.

### The result

The result of the solution is a `pkg_solution_result` object. It is a
named list with entries:

- `status`: Status of the solution attempt, `"OK"` or `"FAILED"`.

- `data`: The selected candidates. This is very similar to a
  [pkg_resolution_result](https://r-lib.github.io/pkgdepends/dev/reference/pkg_resolution.md)
  object, but it has two extra columns:

  - `lib_status`: status of the package in the library, after the
    installation. Possible values: `new` (will be newly installed),
    `current` (up to date, not installed), `update` (will be updated),
    `no-update` (could update, but will not).

  - `old_version`: The old (current) version of the package in the
    library, or `NA` if the package is currently not installed.

- `problem`: The ILP problem. The exact representation is an
  implementation detail, but it does have an informative print method.

- `solution`: The return value of the internal solver.
