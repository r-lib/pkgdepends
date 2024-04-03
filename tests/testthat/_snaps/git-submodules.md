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

