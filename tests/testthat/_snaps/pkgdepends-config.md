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
      env_decode_difftime("", "UPDATE")
    Error <simpleError>
      Invalid time interval specification in `UPDATE` environment variable: ``

---

    Code
      env_decode_difftime("123", "UPDATE")
    Error <simpleError>
      Invalid time interval specification in `UPDATE` environment variable: `123`

---

    Code
      env_decode_difftime("1k", "UPDATE")
    Error <simpleError>
      Invalid time interval specification in `UPDATE` environment variable: `1k`

---

    Code
      env_decode_difftime("k1k", "UPDATE")
    Error <simpleError>
      Invalid time interval specification in `UPDATE` environment variable: `k1k`

# current_config

    Code
      sort(current_config()$list())
    Output
       [1] "build_vignettes"       "cache_dir"             "cran_mirror"          
       [4] "dependencies"          "library"               "metadata_cache_dir"   
       [7] "metadata_update_after" "package_cache_dir"     "platforms"            
      [10] "r_versions"            "sysreqs"               "sysreqs_dry_run"      
      [13] "sysreqs_rspm_repo_id"  "sysreqs_rspm_url"      "sysreqs_sudo"         
      [16] "sysreqs_verbose"       "use_bioconductor"      "windows_archs"        

