# parse_remote_deps

    Code
      parse_remote_deps("deps::/foo/bar")
    Output
      [[1]]
      [[1]]$path
      [1] "/foo/bar"
      
      [[1]]$ref
      [1] "deps::/foo/bar"
      
      [[1]]$type
      [1] "deps"
      
      

---

    Code
      parse_remote_deps("deps::foo/bar")
    Output
      [[1]]
      [[1]]$path
      [1] "foo/bar"
      
      [[1]]$ref
      [1] "deps::foo/bar"
      
      [[1]]$type
      [1] "deps"
      
      

---

    Code
      parse_remote_deps("deps::~foo/bar")
    Output
      [[1]]
      [[1]]$path
      [1] "~foo/bar"
      
      [[1]]$ref
      [1] "deps::~foo/bar"
      
      [[1]]$type
      [1] "deps"
      
      

# resolve_remote_deps

    Code
      prop$get_resolution()[, c("ref", "package", "version")]
    Output
      # A data frame: 5 x 3
        ref      package        version
        <chr>    <chr>          <chr>  
      1 deps::.  mypackage-deps 1.0.0  
      2 local::. mypackage      1.0.0  
      3 pkg1     pkg1           1.0.0  
      4 pkg2     pkg2           1.0.0  
      5 pkg3     pkg3           1.0.0  

---

    Code
      prop$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - deps::.
      + constraints (11):
        - select mypackage-deps exactly once
        - select mypackage at most once
        - select pkg1 at most once
        - select pkg2 at most once
        - select pkg3 at most once
        - deps::. depends on pkg2: version pkg2 1.0.0
        - deps::. depends on pkg3: version pkg3 1.0.0
        - local::. depends on pkg2: version pkg2 1.0.0
        - local::. depends on pkg3: version pkg3 1.0.0
        - pkg2 depends on pkg1: version pkg1 1.0.0
        ...
      + solution:
        - deps::.
        - pkg1
        - pkg2
        - pkg3

