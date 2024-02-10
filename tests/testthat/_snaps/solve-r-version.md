# R version dependencies

    Code
      p1$stop_for_solution_error()
    Condition
      Error:
      ! Could not solve package dependencies:
      * futurama: Needs R >= 3000.0

---

    Code
      p2$stop_for_solution_error()
    Condition
      Error:
      ! Could not solve package dependencies:
      * needsfuturama: Can't install dependency futurama
      * futurama: Needs R >= 3000.0

