# new_pkg_download_proposal

    Code
      dl
    Output
      <pkg_download_proposal>
      + refs:
        - pkg3
      (use `$resolve()` to resolve dependencies)

---

    Code
      dl$get_refs()
    Output
      [1] "pkg3"

---

    Code
      sort(dl$get_config()$list())
    Output
       [1] "build_vignettes"           "cache_dir"                
       [3] "cran_mirror"               "dependencies"             
       [5] "git_submodules"            "goal"                     
       [7] "include_linkingto"         "library"                  
       [9] "metadata_cache_dir"        "metadata_update_after"    
      [11] "package_cache_dir"         "platforms"                
      [13] "r_versions"                "sysreqs"                  
      [15] "sysreqs_db_update"         "sysreqs_db_update_timeout"
      [17] "sysreqs_dry_run"           "sysreqs_lookup_system"    
      [19] "sysreqs_platform"          "sysreqs_rspm_repo_id"     
      [21] "sysreqs_rspm_url"          "sysreqs_sudo"             
      [23] "sysreqs_update"            "sysreqs_verbose"          
      [25] "use_bioconductor"          "windows_archs"            

---

    Code
      dl
    Output
      <pkg_download_proposal>
      + refs:
        - pkg3
      + has resolution (+2 dependencies)
      (use `$download()` to download packages)
      (use `$get_resolution()` to see resolution results)

---

    Code
      dl$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
    Output
      # A data frame: 3 x 5
        ref   type     directpkg package error     
        <chr> <chr>    <lgl>     <chr>   <list>    
      1 pkg1  standard FALSE     pkg1    <list [0]>
      2 pkg2  standard FALSE     pkg2    <list [0]>
      3 pkg3  standard TRUE      pkg3    <list [0]>

# async_resolve

    Code
      synchronize(dl$async_resolve()$then(function() "done"))
    Output
      [1] "done"

---

    Code
      dl$get_resolution()[, c("ref", "type", "directpkg", "package", "error")]
    Output
      # A data frame: 3 x 5
        ref   type     directpkg package error     
        <chr> <chr>    <lgl>     <chr>   <list>    
      1 pkg1  standard FALSE     pkg1    <list [0]>
      2 pkg2  standard FALSE     pkg2    <list [0]>
      3 pkg3  standard TRUE      pkg3    <list [0]>

# download

    Code
      dir(tmp, recursive = TRUE, pattern = "[.]tar[.]gz$")
    Output
      [1] "src/contrib/pkg1_1.0.0.tar.gz" "src/contrib/pkg2_1.0.0.tar.gz"
      [3] "src/contrib/pkg3_1.0.0.tar.gz"

# async_download

    Code
      dir(tmp, recursive = TRUE, pattern = "[.]tar[.]gz$")
    Output
      [1] "src/contrib/pkg1_1.0.0.tar.gz" "src/contrib/pkg2_1.0.0.tar.gz"
      [3] "src/contrib/pkg3_1.0.0.tar.gz"

---

    Code
      dl$get_downloads()$target
    Output
      [1] "src/contrib/pkg1_1.0.0.tar.gz" "src/contrib/pkg2_1.0.0.tar.gz"
      [3] "src/contrib/pkg3_1.0.0.tar.gz"

