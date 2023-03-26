# conflicting dependencies

    Code
      dsc
    Output
      * aa: Can't install dependency cran::cc
      * cran::cc: cran::cc conflicts with cc/cc, to be installed

# conflicting dependencies downstream

    Code
      dsc
    Output
      * a0: Can't install dependency bb
      * bb: Can't install dependency cc/cc
      * cc/cc: cc/cc conflicts with cran::cc, to be installed

# conflicting dependencies and installed packages

    Code
      p$get_solution()$failures
    Output
      * cran/dm@1.0.0: Can't install dependency tidyr
      * tidyr: Can't install dependency dplyr (>= 1.0.10)
      * dplyr: Conflicts with cran/dplyr@1.0.9

---

    Code
      p$get_solution()$failures
    Output
      * cran/dm@1.0.0: Can't install dependency tidyr
      * installed::<path>/tidyr: Can't install dependency dplyr (>= 1.0.10)
      * dplyr: Conflicts with cran/dplyr@1.0.9

---

    Code
      p$get_solution()$failures
    Output
      * local::<path>/<pkg>: dependency conflict

---

    Code
      p2$get_solution()$failures
    Output
      * local::<path>/<pkg>: dependency conflict

