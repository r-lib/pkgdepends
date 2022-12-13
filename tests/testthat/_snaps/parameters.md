# reinstall, standard

    Code
      # pkg1 is already installed
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - pkg1
      + constraints (1):
        - select pkg1 exactly once
      + solution:
        - installed::.../pkg1

---

    Code
      # request a reinstall
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - pkg3
      + constraints (6):
        - select pkg3 exactly once
        - select pkg1 at most once
        - select pkg2 at most once
        - pkg2 depends on pkg1: version pkg1 1.0.0, installed::.../pkg1 1.0.0
        - pkg3 depends on pkg2: version pkg2 1.0.0, installed::.../pkg2 1.0.0
        - installed::.../pkg1 1.0.0
      + solution:
        - installed::.../pkg1
        - installed::.../pkg2
        - pkg3

---

    Code
      # request a reinstall of a dependency
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - pkg3
      + constraints (7):
        - select pkg3 exactly once
        - select pkg1 at most once
        - select pkg2 at most once
        - pkg2 depends on pkg1: version pkg1 1.0.0
        - pkg3 depends on pkg2: version pkg2 1.0.0, installed::.../pkg2 1.0.0
        - installed::.../pkg2 depends on pkg1: version pkg1 1.0.0
        - installed::.../pkg2 1.0.0
      + solution:
        - installed::.../pkg2
        - installed::.../pkg3
        - pkg1

---

    Code
      # one reinstall, one not
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - pkg1
        - pkg3
      + constraints (7):
        - select pkg3 exactly once
        - select pkg1 exactly once
        - select pkg2 at most once
        - pkg2 depends on pkg1: version pkg1 1.0.0, installed::.../pkg1 1.0.0
        - pkg3 depends on pkg2: version pkg2 1.0.0, installed::.../pkg2 1.0.0
        - installed::.../pkg1 1.0.0
        - installed::.../pkg2 1.0.0
      + solution:
        - installed::.../pkg1
        - installed::.../pkg2
        - installed::.../pkg3

# reinstsll from URL

    Code
      # reinstall from direct URL
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz
      + constraints (2):
        - select pkg1 exactly once
        - `url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz` is not satisfied by `installed::.../pkg1`
      + solution:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz

---

    Code
      # reinstall from URL, extra parameters
      p$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz
      + constraints (1):
        - select pkg1 exactly once
      + solution:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz

# source

    Code
      sol[, c("package", "platform")]
    Output
      # A data frame: 2 x 2
        package platform               
      * <chr>   <chr>                  
      1 pkg     source                 
      2 pkg2    i386+x86_64-w64-mingw32

# source for dependency

    Code
      sol[, c("package", "platform")]
    Output
      # A data frame: 2 x 2
        package platform               
      * <chr>   <chr>                  
      1 pkg     i386+x86_64-w64-mingw32
      2 pkg2    source                 

