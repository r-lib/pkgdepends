# can package a tree

    Code
      install_package_plan(plan, lib = lib)
    Message
      i Packaging foo 0.0.0.9000
      v Packaged foo 0.0.0.9000
      i Building foo 0.0.0.9000
      v Built foo 0.0.0.9000
      v Installed foo 0.0.0.9000 (local)
      v Summary:

# can package a compressed tree

    Code
      install_package_plan(plan, lib = lib)
    Message
      i Packaging foo 0.0.0.9000
      v Packaged foo 0.0.0.9000
      i Building foo 0.0.0.9000
      v Built foo 0.0.0.9000
      v Installed foo 0.0.0.9000 (local)
      v Summary:

# can package a source package

    Code
      install_package_plan(plan, lib = lib)
    Message
      i Building foo 0.0.0.9000
      v Built foo 0.0.0.9000
      v Installed foo 0.0.0.9000 (local)
      v Summary:

# add_recursive_dependencies

    Code
      add_recursive_dependencies(plan)
    Output
      # A data frame: 3 x 6
        package type  binary dependencies file  needscompilation
        <chr>   <chr> <lgl>  <list>       <chr> <lgl>           
      1 p1      cran  FALSE  <chr [2]>    <NA>  FALSE           
      2 p2      cran  TRUE   <chr [1]>    <NA>  FALSE           
      3 p3      cran  FALSE  <chr [0]>    <NA>  FALSE           

---

    Code
      add_recursive_dependencies(plan)$dependencies
    Output
      [[1]]
      [1] "p2" "p3"
      
      [[2]]
      [1] "p3"
      
      [[3]]
      character(0)
      

