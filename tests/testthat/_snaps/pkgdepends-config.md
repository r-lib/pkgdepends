# windows_archs

    Code
      windows_archs()
    Output
      [1] "prefer-x64" "both"      

# env_decode_difftime

    Code
      env_decode_difftime("0s")
    Output
      Time difference of 0 secs
    Code
      env_decode_difftime("120s")
    Output
      Time difference of 120 secs
    Code
      env_decode_difftime("0m")
    Output
      Time difference of 0 mins
    Code
      env_decode_difftime("120m")
    Output
      Time difference of 120 mins
    Code
      env_decode_difftime("0h")
    Output
      Time difference of 0 hours
    Code
      env_decode_difftime("24h")
    Output
      Time difference of 24 hours
    Code
      env_decode_difftime("0d")
    Output
      Time difference of 0 days
    Code
      env_decode_difftime("1d")
    Output
      Time difference of 1 days

---

    Code
      env_decode_difftime("", "PKG_UPDATE")
    Error <rlib_error_3_0>
      ! Invalid time interval specification in `PKG_UPDATE` environment variable: ""
      i It must have the form `<number><unit>`.
      i The unit must be a single letter: `s` (seconds), `m` (minutes), `h` (hours) or `d` (days).
      i Examples: 60s, 2h, 1d.

---

    Code
      env_decode_difftime("123", "PKG_UPDATE")
    Error <rlib_error_3_0>
      ! Invalid time interval specification in `PKG_UPDATE` environment variable: "123"
      i It must have the form `<number><unit>`.
      i The unit must be a single letter: `s` (seconds), `m` (minutes), `h` (hours) or `d` (days).
      i Examples: 60s, 2h, 1d.

---

    Code
      env_decode_difftime("1k", "PKG_UPDATE")
    Error <rlib_error_3_0>
      ! Invalid time interval specification in `PKG_UPDATE` environment variable: "1k"
      i It must have the form `<number><unit>`.
      i The unit must be a single letter: `s` (seconds), `m` (minutes), `h` (hours) or `d` (days).
      i Examples: 60s, 2h, 1d.

---

    Code
      env_decode_difftime("k1k", "PKG_UPDATE")
    Error <rlib_error_3_0>
      ! Invalid time interval specification in `PKG_UPDATE` environment variable: "k1k"
      i It must have the form `<number><unit>`.
      i The unit must be a single letter: `s` (seconds), `m` (minutes), `h` (hours) or `d` (days).
      i Examples: 60s, 2h, 1d.

# current_config

    Code
      sort(current_config()$list())
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

