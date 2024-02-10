# new_pkg_installation_proposal

    Code
      prop
    Output
      <pkg_installation_proposal>
      + refs:
        - pkg3
      + solution policy: lazy
      (use `$solve()` to solve dependencies)

---

    Code
      prop$get_refs()
    Output
      [1] "pkg3"

---

    Code
      sort(prop$get_config()$list())
    Output
       [1] "build_vignettes"           "cache_dir"                
       [3] "cran_mirror"               "dependencies"             
       [5] "goal"                      "include_linkingto"        
       [7] "library"                   "metadata_cache_dir"       
       [9] "metadata_update_after"     "package_cache_dir"        
      [11] "platforms"                 "r_versions"               
      [13] "sysreqs"                   "sysreqs_db_update"        
      [15] "sysreqs_db_update_timeout" "sysreqs_dry_run"          
      [17] "sysreqs_lookup_system"     "sysreqs_platform"         
      [19] "sysreqs_rspm_repo_id"      "sysreqs_rspm_url"         
      [21] "sysreqs_sudo"              "sysreqs_update"           
      [23] "sysreqs_verbose"           "use_bioconductor"         
      [25] "windows_archs"            

---

    Code
      prop$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
    Output
      # A data frame: 3 x 5
        ref   type     directpkg package error     
        <chr> <chr>    <lgl>     <chr>   <list>    
      1 pkg1  standard FALSE     pkg1    <list [0]>
      2 pkg2  standard FALSE     pkg2    <list [0]>
      3 pkg3  standard TRUE      pkg3    <list [0]>

---

    Code
      prop$get_solve_policy()
    Output
      [1] "lazy"

---

    Code
      prop$get_solve_policy()
    Output
      [1] "upgrade"

---

    Code
      prop$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - pkg3
      + constraints (5):
        - select pkg3 exactly once
        - select pkg1 at most once
        - select pkg2 at most once
        - pkg2 depends on pkg1: version pkg1 1.0.0
        - pkg3 depends on pkg2: version pkg2 1.0.0
      + solution:
        - pkg1
        - pkg2
        - pkg3

---

    Code
      prop$show_solution()
    Message
      + pkg1   1.0.0 [bld][dl] (<size>)
      + pkg2   1.0.0 [bld][dl] (<size>)
      + pkg3   1.0.0 [bld][dl] (<size>)

---

    Code
      prop$draw()
    Output
      pkg3 1.0.0 [new][bld][dl] (<size>)
      \-pkg2 1.0.0 [new][bld][dl] (<size>)
        \-pkg1 1.0.0 [new][bld][dl] (<size>)
      
      Key:  [new] new | [dl] download | [bld] build

# async_resolve

    Code
      suppressMessages(synchronize(prop$async_resolve()$then(function() "done")))
    Output
      [1] "done"

---

    Code
      prop$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
    Output
      # A data frame: 3 x 5
        ref   type     directpkg package error     
        <chr> <chr>    <lgl>     <chr>   <list>    
      1 pkg1  standard FALSE     pkg1    <list [0]>
      2 pkg2  standard FALSE     pkg2    <list [0]>
      3 pkg3  standard TRUE      pkg3    <list [0]>

# download

    Code
      prop$get_downloads()[, c("target", "error")]
    Output
      # A data frame: 3 x 2
        target                        error     
        <chr>                         <list>    
      1 src/contrib/pkg1_1.0.0.tar.gz <list [0]>
      2 src/contrib/pkg2_1.0.0.tar.gz <list [0]>
      3 src/contrib/pkg3_1.0.0.tar.gz <list [0]>

# async_download

    Code
      prop$get_downloads()[, c("target", "error")]
    Output
      # A data frame: 3 x 2
        target                        error     
        <chr>                         <list>    
      1 src/contrib/pkg1_1.0.0.tar.gz <list [0]>
      2 src/contrib/pkg2_1.0.0.tar.gz <list [0]>
      3 src/contrib/pkg3_1.0.0.tar.gz <list [0]>

# install

    Code
      pkgcache::parse_installed(lib)[, c("Package", "RemoteRepos")]
    Output
      # A data frame: 3 x 2
        Package RemoteRepos            
        <chr>   <chr>                  
      1 pkg1    http://127.0.0.1:<port>/
      2 pkg2    http://127.0.0.1:<port>/
      3 pkg3    http://127.0.0.1:<port>/

# get_install_plan

    Code
      prop$get_install_plan()[, c("package", "direct", "dependencies")]
    Output
      # A data frame: 3 x 3
        package direct dependencies
        <chr>   <lgl>  <I<list>>   
      1 pkg1    FALSE  <chr [0]>   
      2 pkg2    FALSE  <chr [1]>   
      3 pkg3    TRUE   <chr [1]>   

---

    Code
      prop$get_install_plan()[["dependencies"]]
    Output
      [[1]]
      character(0)
      
      [[2]]
      [1] "pkg1"
      
      [[3]]
      [1] "pkg2"
      

# show_sysreqs

    Code
      prop$show_sysreqs()
    Message
      * libcurl4-openssl-dev  - curl
      * libssl-dev            - curl

# install_sysreqs

    Code
      prop$install_sysreqs()
    Message
      i Installing system requirements
      i Executing `sh -c echo apt-get -y update`
      i Executing `sh -c echo apt-get -y install libcurl4-openssl-dev libssl-dev`

---

    Code
      prop$install_sysreqs()
    Output
      NULL

---

    Code
      prop$install_sysreqs()
    Output
      NULL

