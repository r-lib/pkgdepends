# pkg_dep_types_*

    Code
      pkg_dep_types_hard()
    Output
      [1] "Depends"   "Imports"   "LinkingTo"

---

    Code
      pkg_dep_types_soft()
    Output
      [1] "Suggests" "Enhances"

---

    Code
      pkg_dep_types()
    Output
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 

# parse_deps

    Code
      parse_deps(character(), character())
    Output
      list()

---

    Code
      parse_deps("foo", "Imports")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports foo     ""    ""     
      

---

    Code
      parse_deps(c("foo", "foo"), c("Imports", "LinkingTo"))
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports foo     ""    ""     
      
      [[2]]
      # A data frame: 1 x 4
        type      package op    version
      * <chr>     <chr>   <chr> <chr>  
      1 LinkingTo foo     ""    ""     
      

---

    Code
      parse_deps("foo (>= 1.0.0)", "Imports")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports foo     >=    1.0.0  
      

---

    Code
      parse_deps("foo (>= 1.0.0), bar", "Imports")
    Output
      [[1]]
      # A data frame: 2 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports foo     ">="  "1.0.0"
      2 Imports bar     ""    ""     
      

---

    Code
      parse_deps("foo (>= 1.0.0)\n bar", "Imports")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports <NA>    <NA>  <NA>   
      

---

    Code
      parse_deps("foo (>= \n 1.0.0)\n bar", "Imports")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports <NA>    <NA>  <NA>   
      

---

    Code
      parse_deps("R (== 4.2.1)", "Depends")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Depends R       ==    4.2.1  
      

---

    Code
      parse_deps("R (< 4.2.1)", "Depends")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Depends R       <     4.2.1  
      

---

    Code
      parse_deps("R (> 4.2.1)", "Depends")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Depends R       >     4.2.1  
      

---

    Code
      parse_deps("R (<= 4.2.1)", "Depends")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Depends R       <=    4.2.1  
      

---

    Code
      parse_deps("foo, stats", "Imports")
    Output
      [[1]]
      # A data frame: 1 x 4
        type    package op    version
      * <chr>   <chr>   <chr> <chr>  
      1 Imports foo     ""    ""     
      

# resolve_ref_deps

    Code
      resolve_ref_deps(dsc$get_deps(), NA_character_, NULL)
    Output
      # A data frame: 7 x 5
        ref        type     package    op    version
        <chr>      <chr>    <chr>      <chr> <chr>  
      1 covr       Suggests covr       ""    ""     
      2 jsonlite   Suggests jsonlite   ""    ""     
      3 testthat   Suggests testthat   ">="  "3.1.0"
      4 assertthat Imports  assertthat ""    ""     
      5 curl       Imports  curl       ""    ""     
      6 R6         Imports  R6         ""    ""     
      7 rlang      Imports  rlang      ">="  "1.0.0"

---

    Code
      resolve_ref_deps(dsc$get_deps(), dsc$get("Remotes"), dsc$get(
        extra_config_fields(dsc$fields())))
    Output
      # A data frame: 9 x 5
        ref           type                 package    op    version
        <chr>         <chr>                <chr>      <chr> <chr>  
      1 r-lib/covr    Suggests             covr       ""    ""     
      2 jsonlite      Suggests             jsonlite   ""    ""     
      3 testthat      Suggests             testthat   ">="  "3.1.0"
      4 assertthat    Imports              assertthat ""    ""     
      5 curl          Imports              curl       ""    ""     
      6 R6            Imports              R6         ""    ""     
      7 rlang         Imports              rlang      ">="  "1.0.0"
      8 pkgdown       Config/Needs/website pkgdown    ""    ""     
      9 r-lib/downlit Config/Needs/website downlit    ""    ""     

# resolve_ref_deps, package name from remote

    Code
      resolve_ref_deps(dsc$get_deps(), dsc$get("Remotes"), dsc$get(
        extra_config_fields(dsc$fields())))
    Output
      # A data frame: 2 x 5
        ref              type                 package op    version
        <chr>            <chr>                <chr>   <chr> <chr>  
      1 foo=r-lib/bar    Imports              foo     ""    ""     
      2 foobar=r-lib/baz Config/Needs/website foobar  ""    ""     

---

    Code
      resolve_ref_deps(dsc$get_deps(), dsc$get("Remotes"), NULL)
    Error <rlib_error_3_0>
      ! Cannot determine package name for 1 package: "url::http://example.com".
      i Maybe you need to add a `<packagename>=` prefix?

# as_pkg_dependencies

    Code
      as_pkg_dependencies(TRUE)
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests" 
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo"
      

---

    Code
      as_pkg_dependencies("all")
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo"
      

---

    Code
      as_pkg_dependencies("hard")
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo"
      

---

    Code
      as_pkg_dependencies(FALSE)
    Output
      $direct
      character(0)
      
      $indirect
      character(0)
      

---

    Code
      as_pkg_dependencies(NA)
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo"
      

---

    Code
      as_pkg_dependencies(list(direct = "Imports", indirect = c("Imports", "Suggests")))
    Output
      $direct
      [1] "Imports"
      
      $indirect
      [1] "Imports"  "Suggests"
      

---

    Code
      as_pkg_dependencies(c("Depends", "Imports", "LinkingTo"))
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo"
      

---

    Code
      as_pkg_dependencies(list(direct = "all", indirect = "hard"))
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo"
      

---

    Code
      as_pkg_dependencies(list(direct = "all", indirect = "all"))
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 
      

---

    Code
      as_pkg_dependencies(list(direct = "all", indirect = c("hard", "soft")))
    Output
      $direct
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 
      
      $indirect
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests"  "Enhances" 
      

