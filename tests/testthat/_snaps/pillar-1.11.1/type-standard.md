# resolve_remote

    Code
      as.list(res[, intersect(names(res), cols)])
    Output
      $package
      [1] "crayon"
      
      $version
      [1] "1.0.0"
      
      $needscompilation
      [1] FALSE
      
      $repodir
      [1] "src/contrib"
      
      $rversion
      [1] "*"
      
      $platform
      [1] "source"
      
      $priority
      [1] NA
      
      $ref
      [1] "crayon"
      
      $type
      [1] "standard"
      
      $status
      [1] "OK"
      
      $target
      [1] "src/contrib/crayon_1.0.0.tar.gz"
      
      $mirror
      [1] "http://127.0.0.1:<port>/"
      
      $sources
      $sources[[1]]
      [1] "http://127.0.0.1:<port>//src/contrib/crayon_1.0.0.tar.gz"               
      [2] "http://127.0.0.1:<port>//src/contrib/Archive/crayon/crayon_1.0.0.tar.gz"
      
      
      $sysreqs
      [1] NA
      
      $deps
      $deps[[1]]
      # A data frame: 0 x 5
      # i 5 variables: ref <chr>, type <chr>, package <chr>, op <chr>, version <chr>
      
      
      $built
      [1] NA
      
      $repotype
      [1] "cran"
      
      $direct
      [1] TRUE
      
      $params
      $params[[1]]
      character(0)
      
      
      $metadata
      $metadata[[1]]
                     RemoteType              RemotePkgRef                 RemoteRef 
                     "standard"                  "crayon"                  "crayon" 
                    RemoteRepos         RemotePkgPlatform                 RemoteSha 
      "http://127.0.0.1:<port>/"                  "source"                   "1.0.0" 
      
      

---

    Code
      as.list(res[, intersect(names(res), cols)])
    Output
      $ref
      [1] "thispackagecannotexistforsure"
      
      $type
      [1] "standard"
      
      $direct
      [1] TRUE
      
      $status
      [1] "FAILED"
      
      $package
      [1] "thispackagecannotexistforsure"
      
      $version
      [1] NA
      
      $needscompilation
      [1] TRUE
      
      $priority
      [1] NA
      
      $built
      [1] NA
      
      $platform
      [1] "source"
      
      $rversion
      [1] "*"
      
      $repotype
      [1] NA
      
      $repodir
      [1] "src/contrib"
      
      $target
      [1] "src/contrib/thispackagecannotexistforsure_NA.tar.gz"
      
      $deps
      $deps[[1]]
      # A data frame: 0 x 5
      # i 5 variables: ref <chr>, type <chr>, package <chr>, op <chr>, version <chr>
      
      
      $mirror
      [1] NA
      
      $sources
      $sources[[1]]
      [1] NA
      
      
      $metadata
      $metadata[[1]]
      list()
      
      
      $params
      $params[[1]]
      character(0)
      
      
      $sysreqs
      [1] NA
      

