# different versions required

    Code
      p$stop_for_solution_error()
    Error <rlib_error_3_0>
      ! Cannot install packages:
      * r-lib/crayon: Conflicts with crayon
      * crayon: Conflicts with r-lib/crayon

# direct CRAN conflicts with downstream GH dep

    Code
      p$stop_for_solution_error()
    Error <rlib_error_3_0>
      ! Cannot install packages:
      * r-lib/foo: Can't install dependency r-lib/crayon
      * r-lib/crayon: Conflicts with crayon

# no required version

    Code
      p$stop_for_solution_error()
    Error <rlib_error_3_0>
      ! Cannot install packages:
      * pkg1: Can't install dependency pkg2 (>= 2.0.0)

# failed resolution

    Code
      p1$stop_for_solution_error()
    Error <rlib_error_3_0>
      ! Cannot install packages:
      * SDF: Can't find package called SDF.

---

    Code
      p2$stop_for_solution_error()
    Error <rlib_error_3_0>
      ! Cannot install packages:
      * SDF/SDF: ! Can't find GitHub repo SDF/SDF.

