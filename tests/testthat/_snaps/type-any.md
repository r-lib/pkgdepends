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
      

