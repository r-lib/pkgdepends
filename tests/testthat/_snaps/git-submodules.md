# parse_submodules

    Code
      parse_submodules(sm)
    Output
                              submodule                            path
      1 dependencies/fast_double_parser dependencies/fast_double_parser
      2                dependencies/fmt                dependencies/fmt
      3         dependencies/boost_math         dependencies/boost_math
      4              dependencies/eigen              dependencies/eigen
                                                  url branch
      1 https://github.com/lemire/fast_double_parser/   <NA>
      2                https://github.com/fmtlib/fmt/   <NA>
      3              https://github.com/boostorg/math   <NA>
      4             https://gitlab.com/libeigen/eigen   <NA>
    Code
      parse_submodules(read_char(sm))
    Output
                              submodule                            path
      1 dependencies/fast_double_parser dependencies/fast_double_parser
      2                dependencies/fmt                dependencies/fmt
      3         dependencies/boost_math         dependencies/boost_math
      4              dependencies/eigen              dependencies/eigen
                                                  url branch
      1 https://github.com/lemire/fast_double_parser/   <NA>
      2                https://github.com/fmtlib/fmt/   <NA>
      3              https://github.com/boostorg/math   <NA>
      4             https://gitlab.com/libeigen/eigen   <NA>

---

    Code
      parse_submodules(tmp)
    Output
      list()

---

    Code
      parse_submodules(sm2)
    Condition
      Warning:
      Invalid submodule definition, skipping submodule installation
    Output
      list()
    Code
      parse_submodules(sm3)
    Condition
      Warning:
      Invalid submodule definition, skipping submodule installation
    Output
      list()

# git_download_repo with submodules

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "v1/DESCRIPTION"   "v1/NAMESPACE"     "v1/R/foo.R"       "v1/README.md"    
      [5] "v1/submod/README" "v1/wipe.R"       

---

    Code
      readLines(file.path(output, "submod", "README"))
    Output
      [1] "A git submodule" "Another commit" 

---

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "v1/DESCRIPTION"   "v1/NAMESPACE"     "v1/R/foo.R"       "v1/README.md"    
      [5] "v1/submod/README" "v1/wipe.R"       

---

    Code
      readLines(file.path(output, "submod", "README"))
    Output
      [1] "A git submodule" "Another commit"  "Third commit"   

# git_download_repo R package with submodules

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "v1/DESCRIPTION"   "v1/NAMESPACE"     "v1/R/foo.R"       "v1/README.md"    
      [5] "v1/submod/README" "v1/wipe.R"       

---

    Code
      readLines(file.path(output, "submod", "README"))
    Output
      [1] "A git submodule" "Another commit" 

---

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "v1/DESCRIPTION"   "v1/NAMESPACE"     "v1/R/foo.R"       "v1/README.md"    
      [5] "v1/submod/README" "v1/wipe.R"       

---

    Code
      readLines(file.path(output, "submod", "README"))
    Output
      [1] "A git submodule" "Another commit"  "Third commit"   

---

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "v1/DESCRIPTION" "v1/NAMESPACE"   "v1/R/foo.R"     "v1/README.md"  
      [5] "v1/wipe.R"     

---

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "v1/DESCRIPTION" "v1/NAMESPACE"   "v1/R/foo.R"     "v1/README.md"  
      [5] "v1/wipe.R"     

# git_download_repo R package with ignored submodule

    Code
      dir(tmp, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    Output
       [1] "v1/.Rbuildignore"                        
       [2] "v1/.github/actions/parameters/action.yml"
       [3] "v1/.github/workflows/inputtest.yml"      
       [4] "v1/.gitignore"                           
       [5] "v1/.gitmodules"                          
       [6] "v1/DESCRIPTION"                          
       [7] "v1/NAMESPACE"                            
       [8] "v1/R/foo.R"                              
       [9] "v1/README.md"                            
      [10] "v1/wipe.R"                               

# directories

    Code
      directories("a")
    Output
      character(0)
    Code
      directories("a/b/c/d")
    Output
      [1] "a"     "a/b"   "a/b/c"

