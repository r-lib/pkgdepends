# download_remote error

    Code
      r$resolve()
    Output
      # A tibble: 1 x 29
        ref     type  direct directpkg status package version license needscompilation
        <chr>   <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>   <lgl>           
      1 local:~ local TRUE   TRUE      OK     foobar  1.0.0   MIT + ~ FALSE           
      # ... with 20 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,
      #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,
      #   repotype <chr>, repodir <chr>, target <chr>, deps <list>, mirror <chr>,
      #   sources <list>, remote <list>, error <list>, metadata <list>, extra <list>,
      #   dep_types <list>, params <list>, cache_status <chr>

---

    Code
      r$download_resolution()
    Message <cliMessage>
      i Getting 1 pkg with unknown size
      x Failed to download foobar 1.0.0 (source)
    Output
      # A tibble: 1 x 34
        ref     type  direct directpkg status package version license needscompilation
        <chr>   <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>   <lgl>           
      1 local:~ local TRUE   TRUE      OK     foobar  1.0.0   MIT + ~ FALSE           
      # ... with 25 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,
      #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,
      #   repotype <chr>, repodir <chr>, target <chr>, deps <list>, mirror <chr>,
      #   sources <list>, remote <list>, error <list>, metadata <list>, extra <list>,
      #   dep_types <list>, params <list>, cache_status <chr>, fulltarget <chr>,
      #   fulltarget_tree <chr>, download_status <chr>, download_error <list>,
      #   file_size <dbl>

