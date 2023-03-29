# parse_remote_any

    Code
      plan$draw()
    Output
      any::pkg3 1.0.0 [new][bld][dl] (<size>)
      \-pkg2 1.0.0 [new][bld][dl] (<size>)
        \-pkg1 1.0.0 [new][bld][dl] (<size>)
      
      Key:  [new] new | [dl] download | [bld] build

---

    Code
      plan$draw()
    Output
      pkg3 1.0.0 
      \-pkg2 1.0.0 
        \-pkg1 1.0.0 
      

# dep_types

    Code
      as.list(res[c("ref", "dep_types")])
    Output
      $ref
      [1] "any::pkg3" "pkg1"      "pkg2"      "pkg3"     
      
      $dep_types
      $dep_types[[1]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[2]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[3]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[4]]
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests" 
      
      

---

    Code
      as.list(res[c("ref", "dep_types")])
    Output
      $ref
      [1] "any::pkg3"              "installed::<path>/pkg1" "installed::<path>/pkg2"
      [4] "installed::<path>/pkg3" "pkg1"                   "pkg2"                  
      [7] "pkg3"                  
      
      $dep_types
      $dep_types[[1]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[2]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[3]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[4]]
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests" 
      
      $dep_types[[5]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[6]]
      [1] "Depends"   "Imports"   "LinkingTo"
      
      $dep_types[[7]]
      [1] "Depends"   "Imports"   "LinkingTo" "Suggests" 
      
      

