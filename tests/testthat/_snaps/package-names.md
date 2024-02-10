# local dep needs name

    Code
      p$stop_for_solution_error()
    Condition
      Error:
      ! Could not solve package dependencies:
      * local::./pkg1: ! pkgdepends resolution error for local::./pkg1.
      Caused by error:
      ! Cannot determine package name for 1 package: "local::./pkg2".
      i Maybe you need to add a `<packagename>=` prefix?

---

    Code
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - local::./pkg1
      + constraints (3):
        - select pkg1 exactly once
        - select pkg2 at most once
        - local::./pkg1 depends on pkg2=local::./pkg2: version pkg2=local::./pkg2 1.0.0
      + solution:
        - local::./pkg1
        - pkg2=local::./pkg2

# url dep needs name

    Code
      p$stop_for_solution_error()
    Condition
      Error:
      ! Could not solve package dependencies:
      * local::./pkg2: ! pkgdepends resolution error for local::./pkg2.
      Caused by error:
      ! Cannot determine package name for 1 package: "url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz".
      i Maybe you need to add a `<packagename>=` prefix?

---

    Code
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - local::./pkg2
      + constraints (3):
        - select pkg2 exactly once
        - select pkg1 at most once
        - local::./pkg2 depends on pkg1=url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz: version pkg1=url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz 0.9.0
      + solution:
        - local::./pkg2
        - pkg1=url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz

