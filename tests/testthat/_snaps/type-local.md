# resolve_remote

    Code
      res
    Output
      $ref
      [1] "local::fixtures/foobar_1.0.0.tar.gz"
      
      $type
      [1] "local"
      
      $direct
      [1] TRUE
      
      $status
      [1] "OK"
      
      $package
      [1] "foobar"
      
      $version
      [1] "1.0.0"
      
      $license
      [1] "MIT + file LICENSE"
      
      $needscompilation
      [1] FALSE
      
      $md5sum
      [1] NA
      
      $built
      [1] NA
      
      $platform
      [1] "source"
      
      $rversion
      [1] "*"
      
      $deps
      $deps[[1]]
      # A data frame: 1 x 5
        ref      type     package  op    version
        <chr>    <chr>    <chr>    <chr> <chr>  
      1 testthat Suggests testthat ""    ""     
      
      
      $sources
      [1] "file://fixtures/foobar_1.0.0.tar.gz"
      
      $remote
      $remote[[1]]
      $path
      [1] "fixtures/foobar_1.0.0.tar.gz"
      
      $ref
      [1] "local::fixtures/foobar_1.0.0.tar.gz"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
      
      
      $unknown_deps
      character(0)
      
      $extra
      $extra[[1]]
      $extra[[1]]$description
      Package: foobar
      Title: Foobar Package
      Version: 1.0.0
      Author: Gabor Csardi
      Maintainer: Gabor Csardi <csardi.gabor@gmail.com>
      Description: Just a package to use in test cases.
      License: MIT + file LICENSE
      URL: https://github.com/gaborcsardi/foobar
      BugReports: https://github.com/gaborcsardi/foobar/issues
      Suggests:
          testthat
      LazyData: true
      NeedsCompilation: no
      Packaged: 2017-12-25 19:18:10 UTC; gaborcsardi
      RoxygenNote: 6.0.1
      
      
      
      $metadata
      $metadata$RemotePkgRef
      [1] "local::fixtures/foobar_1.0.0.tar.gz"
      
      $metadata$RemoteType
      [1] "local"
      
      
      $params
      $params[[1]]
      character(0)
      
      
      $sysreqs
      [1] ""
      

---

    Code
      res
    Output
      $ref
      [1] "local::foobar_1.0.0.tar.gz"
      
      $type
      [1] "local"
      
      $direct
      [1] TRUE
      
      $status
      [1] "OK"
      
      $package
      [1] "foobar"
      
      $version
      [1] "1.0.0"
      
      $license
      [1] "MIT + file LICENSE"
      
      $needscompilation
      [1] FALSE
      
      $md5sum
      [1] NA
      
      $built
      [1] NA
      
      $platform
      [1] "source"
      
      $rversion
      [1] "*"
      
      $deps
      $deps[[1]]
      # A data frame: 1 x 5
        ref      type     package  op    version
        <chr>    <chr>    <chr>    <chr> <chr>  
      1 testthat Suggests testthat ""    ""     
      
      
      $sources
      [1] "file://fixtures/foobar_1.0.0.tar.gz"
      
      $remote
      $remote[[1]]
      $path
      [1] "foobar_1.0.0.tar.gz"
      
      $ref
      [1] "local::foobar_1.0.0.tar.gz"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
      
      
      $unknown_deps
      character(0)
      
      $extra
      $extra[[1]]
      $extra[[1]]$description
      Package: foobar
      Title: Foobar Package
      Version: 1.0.0
      Author: Gabor Csardi
      Maintainer: Gabor Csardi <csardi.gabor@gmail.com>
      Description: Just a package to use in test cases.
      License: MIT + file LICENSE
      URL: https://github.com/gaborcsardi/foobar
      BugReports: https://github.com/gaborcsardi/foobar/issues
      Suggests:
          testthat
      LazyData: true
      NeedsCompilation: no
      Packaged: 2017-12-25 19:18:10 UTC; gaborcsardi
      RoxygenNote: 6.0.1
      
      
      
      $metadata
      $metadata$RemotePkgRef
      [1] "local::foobar_1.0.0.tar.gz"
      
      $metadata$RemoteType
      [1] "local"
      
      
      $params
      $params[[1]]
      character(0)
      
      
      $sysreqs
      [1] ""
      

# download_remote error

    Code
      invisible(r$download_resolution())
    Message <cliMessage>
      i Getting 1 pkg with unknown size
      x Failed to download foobar 1.0.0 (source)

