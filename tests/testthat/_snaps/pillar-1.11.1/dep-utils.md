# make_null_deps

    Code
      make_null_deps()
    Output
      # A data frame: 0 x 5
      # i 5 variables: ref <chr>, type <chr>, package <chr>, op <chr>, version <chr>

# parse_deps

    Code
      parse_deps("grid, stats", "Imports")
    Output
      [[1]]
      # A data frame: 0 x 4
      # i 4 variables: type <chr>, package <chr>, op <chr>, version <chr>
      

# parse_all_deps

    Code
      parse_all_deps(c(Imports = NA_character_))
    Output
      # A data frame: 0 x 5
      # i 5 variables: ref <chr>, type <chr>, package <chr>, op <chr>, version <chr>

---

    Code
      parse_all_deps(c(Imports = "grid"))
    Output
      # A data frame: 0 x 5
      # i 5 variables: ref <chr>, type <chr>, package <chr>, op <chr>, version <chr>

---

    Code
      parse_all_deps(c(Imports = "foo (>= 1.0.0), bar", Suggests = "baz, foobaz (>= 2.0.0)"))
    Output
      # A data frame: 4 x 5
        ref    type     package op    version
        <chr>  <chr>    <chr>   <chr> <chr>  
      1 foo    Imports  foo     ">="  "1.0.0"
      2 bar    Imports  bar     ""    ""     
      3 baz    Suggests baz     ""    ""     
      4 foobaz Suggests foobaz  ">="  "2.0.0"

