# resolve_remote

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 1 x 30
        ref   type     direct directpkg status package version license
        <chr> <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 pkg1  standard TRUE   TRUE      OK     pkg1    1.0.0   <NA>   
        needscompilation priority md5sum sha256   filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>       <int> <chr> <chr>    <chr>   
      1 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
        repotype repodir     target                        deps         
        <chr>    <chr>       <chr>                         <list>       
      1 cran     src/contrib src/contrib/pkg1_1.0.0.tar.gz <tbl [0 x 5]>
        mirror                  sources   remote         error      metadata 
        <chr>                   <list>    <list>         <list>     <list>   
      1 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_s [6]> <list [0]> <chr [6]>
        extra      dep_types params    sysreqs cache_status
        <list>     <list>    <list>    <chr>   <chr>       
      1 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      + sources:
      http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz
      + remote:
      <remote_ref_standard/remote_ref/list> package: pkg1; atleast: ; version: ; ref: pkg1; type: standard; params: 
      + error:
      -
      + metadata:
      RemoteType: standard; RemotePkgRef: pkg1; RemoteRef: pkg1; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      + dep_types:
      Depends, Imports, LinkingTo

# resolve_remote, multiple

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 3 x 30
        ref        type     direct directpkg status package version license
        <chr>      <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 pkg2       standard FALSE  FALSE     OK     pkg2    1.0.0   <NA>   
      2 cran::pkg3 cran     TRUE   TRUE      OK     pkg3    1.0.0   <NA>   
      3 pkg1       standard TRUE   TRUE      OK     pkg1    1.0.0   <NA>   
        needscompilation priority md5sum sha256   filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>       <int> <chr> <chr>    <chr>   
      1 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
      2 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
      3 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
        repotype repodir     target                        deps         
        <chr>    <chr>       <chr>                         <list>       
      1 cran     src/contrib src/contrib/pkg2_1.0.0.tar.gz <tbl [1 x 5]>
      2 cran     src/contrib src/contrib/pkg3_1.0.0.tar.gz <tbl [1 x 5]>
      3 cran     src/contrib src/contrib/pkg1_1.0.0.tar.gz <tbl [0 x 5]>
        mirror                  sources   remote         error      metadata 
        <chr>                   <list>    <list>         <list>     <list>   
      1 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_s [6]> <list [0]> <chr [6]>
      2 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_c [6]> <list [0]> <chr [6]>
      3 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_s [6]> <list [0]> <chr [6]>
        extra      dep_types params    sysreqs cache_status
        <list>     <list>    <list>    <chr>   <chr>       
      1 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      2 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      3 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      + sources:
      http://127.0.0.1:<port>//src/contrib/pkg2_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg2/pkg2_1.0.0.tar.gz
      http://127.0.0.1:<port>//src/contrib/pkg3_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg3/pkg3_1.0.0.tar.gz
      http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz
      + remote:
      <remote_ref_standard/remote_ref/list> package: pkg2; atleast: ; version: ; ref: pkg2; type: standard; params: 
      <remote_ref_cran/remote_ref/list> package: pkg3; atleast: ; version: ; ref: cran::pkg3; type: cran; params: 
      <remote_ref_standard/remote_ref/list> package: pkg1; atleast: ; version: ; ref: pkg1; type: standard; params: 
      + error:
      -
      -
      -
      + metadata:
      RemoteType: standard; RemotePkgRef: pkg2; RemoteRef: pkg2; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      RemoteType: cran; RemotePkgRef: cran::pkg3; RemoteRef: cran::pkg3; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      RemoteType: standard; RemotePkgRef: pkg1; RemoteRef: pkg1; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      + dep_types:
      Depends, Imports, LinkingTo
      Depends, Imports, LinkingTo
      Depends, Imports, LinkingTo

# dependencies

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 3 x 30
        ref   type     direct directpkg status package version license
        <chr> <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 pkg1  standard FALSE  FALSE     OK     pkg1    1.0.0   <NA>   
      2 pkg2  standard FALSE  FALSE     OK     pkg2    1.0.0   <NA>   
      3 pkg3  standard TRUE   TRUE      OK     pkg3    1.0.0   <NA>   
        needscompilation priority md5sum sha256   filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>       <int> <chr> <chr>    <chr>   
      1 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
      2 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
      3 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
        repotype repodir     target                        deps         
        <chr>    <chr>       <chr>                         <list>       
      1 cran     src/contrib src/contrib/pkg1_1.0.0.tar.gz <tbl [0 x 5]>
      2 cran     src/contrib src/contrib/pkg2_1.0.0.tar.gz <tbl [1 x 5]>
      3 cran     src/contrib src/contrib/pkg3_1.0.0.tar.gz <tbl [1 x 5]>
        mirror                  sources   remote         error      metadata 
        <chr>                   <list>    <list>         <list>     <list>   
      1 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_s [6]> <list [0]> <chr [6]>
      2 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_s [6]> <list [0]> <chr [6]>
      3 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_s [6]> <list [0]> <chr [6]>
        extra      dep_types params    sysreqs cache_status
        <list>     <list>    <list>    <chr>   <chr>       
      1 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      2 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      3 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      + sources:
      http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz
      http://127.0.0.1:<port>//src/contrib/pkg2_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg2/pkg2_1.0.0.tar.gz
      http://127.0.0.1:<port>//src/contrib/pkg3_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg3/pkg3_1.0.0.tar.gz
      + remote:
      <remote_ref_standard/remote_ref/list> package: pkg1; atleast: ; version: ; ref: pkg1; type: standard; params: 
      <remote_ref_standard/remote_ref/list> package: pkg2; atleast: ; version: ; ref: pkg2; type: standard; params: 
      <remote_ref_standard/remote_ref/list> package: pkg3; atleast: ; version: ; ref: pkg3; type: standard; params: 
      + error:
      -
      -
      -
      + metadata:
      RemoteType: standard; RemotePkgRef: pkg1; RemoteRef: pkg1; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      RemoteType: standard; RemotePkgRef: pkg2; RemoteRef: pkg2; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      RemoteType: standard; RemotePkgRef: pkg3; RemoteRef: pkg3; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      + dep_types:
      Depends, Imports, LinkingTo
      Depends, Imports, LinkingTo
      Depends, Imports, LinkingTo

# failed resolution

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 1 x 30
        ref                      type  direct directpkg status package           
        <chr>                    <chr> <lgl>  <lgl>     <chr>  <chr>             
      1 cran::xxyyzzqwertyqwerty cran  TRUE   TRUE      FAILED xxyyzzqwertyqwerty
        version license needscompilation priority md5sum sha256 filesize built
        <chr>   <chr>   <lgl>            <chr>    <chr>  <chr>     <int> <chr>
      1 <NA>    <NA>    TRUE             <NA>     <NA>   <NA>         NA <NA> 
        platform rversion repotype repodir    
        <chr>    <chr>    <chr>    <chr>      
      1 source   *        <NA>     src/contrib
        target                                   deps          mirror sources  
        <chr>                                    <list>        <chr>  <list>   
      1 src/contrib/xxyyzzqwertyqwerty_NA.tar.gz <tbl [0 x 5]> <NA>   <chr [1]>
        remote         error   metadata   extra      dep_types params    sysreqs
        <list>         <list>  <list>     <list>     <list>    <list>    <chr>  
      1 <rmt_rf_c [6]> <error> <list [0]> <list [0]> <chr [3]> <chr [0]> <NA>   
        cache_status
        <chr>       
      1 miss        
      + sources:
      NA
      + remote:
      <remote_ref_cran/remote_ref/list> package: xxyyzzqwertyqwerty; atleast: ; version: ; ref: cran::xxyyzzqwertyqwerty; type: cran; params: 
      + error:
      <error/condition> Can't find package called cran::xxyyzzqwertyqwerty.
      + metadata:
      -
      + dep_types:
      Depends, Imports, LinkingTo

# failed resolution, multiple

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 2 x 30
        ref                      type  direct directpkg status package           
        <chr>                    <chr> <lgl>  <lgl>     <chr>  <chr>             
      1 cran::pkg1               cran  TRUE   TRUE      OK     pkg1              
      2 cran::xxyyzzqwertyqwerty cran  TRUE   TRUE      FAILED xxyyzzqwertyqwerty
        version license needscompilation priority md5sum sha256   filesize built
        <chr>   <chr>   <lgl>            <chr>    <chr>  <chr>       <int> <chr>
      1 1.0.0   <NA>    FALSE            <NA>     <md5>  <sha256>       42 <NA> 
      2 <NA>    <NA>    TRUE             <NA>     <NA>   <NA>           NA <NA> 
        platform rversion repotype repodir    
        <chr>    <chr>    <chr>    <chr>      
      1 source   *        cran     src/contrib
      2 source   *        <NA>     src/contrib
        target                                   deps          mirror                 
        <chr>                                    <list>        <chr>                  
      1 src/contrib/pkg1_1.0.0.tar.gz            <tbl [0 x 5]> http://127.0.0.1:<port>/
      2 src/contrib/xxyyzzqwertyqwerty_NA.tar.gz <tbl [0 x 5]> <NA>                   
        sources   remote         error      metadata   extra      dep_types params   
        <list>    <list>         <list>     <list>     <list>     <list>    <list>   
      1 <chr [2]> <rmt_rf_c [6]> <list [0]> <chr [6]>  <list [0]> <chr [3]> <chr [0]>
      2 <chr [1]> <rmt_rf_c [6]> <error>    <list [0]> <list [0]> <chr [3]> <chr [0]>
        sysreqs cache_status
        <chr>   <chr>       
      1 <NA>    miss        
      2 <NA>    miss        
      + sources:
      http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz
      NA
      + remote:
      <remote_ref_cran/remote_ref/list> package: pkg1; atleast: ; version: ; ref: cran::pkg1; type: cran; params: 
      <remote_ref_cran/remote_ref/list> package: xxyyzzqwertyqwerty; atleast: ; version: ; ref: cran::xxyyzzqwertyqwerty; type: cran; params: 
      + error:
      -
      <error/condition> Can't find package called cran::xxyyzzqwertyqwerty.
      + metadata:
      RemoteType: cran; RemotePkgRef: cran::pkg1; RemoteRef: cran::pkg1; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      -
      + dep_types:
      Depends, Imports, LinkingTo
      Depends, Imports, LinkingTo

# resolve current version

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 1 x 30
        ref                type  direct directpkg status package version license
        <chr>              <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 cran::pkg1@current cran  TRUE   TRUE      OK     pkg1    1.0.0   <NA>   
        needscompilation priority md5sum sha256   filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>       <int> <chr> <chr>    <chr>   
      1 FALSE            <NA>     <md5>  <sha256>       42 <NA>  source   *       
        repotype repodir     target                        deps         
        <chr>    <chr>       <chr>                         <list>       
      1 cran     src/contrib src/contrib/pkg1_1.0.0.tar.gz <tbl [0 x 5]>
        mirror                  sources   remote         error      metadata 
        <chr>                   <list>    <list>         <list>     <list>   
      1 http://127.0.0.1:<port>/ <chr [2]> <rmt_rf_c [6]> <list [0]> <chr [6]>
        extra      dep_types params    sysreqs cache_status
        <list>     <list>    <list>    <chr>   <chr>       
      1 <list [0]> <chr [3]> <chr [0]> <NA>    miss        
      + sources:
      http://127.0.0.1:<port>//src/contrib/pkg1_1.0.0.tar.gz, http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz
      + remote:
      <remote_ref_cran/remote_ref/list> package: pkg1; atleast: ; version: current; ref: cran::pkg1@current; type: cran; params: 
      + error:
      -
      + metadata:
      RemoteType: cran; RemotePkgRef: cran::pkg1@current; RemoteRef: cran::pkg1@current; RemoteRepos: http://127.0.0.1:<port>/; RemotePkgPlatform: source; RemoteSha: 1.0.0
      + dep_types:
      Depends, Imports, LinkingTo

# resolve an old version

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 1 x 30
        ref        type     direct directpkg status package version license
        <chr>      <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 pkg1@0.9.0 standard TRUE   TRUE      FAILED pkg1    <NA>    <NA>   
        needscompilation priority md5sum sha256 filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>     <int> <chr> <chr>    <chr>   
      1 TRUE             <NA>     <NA>   <NA>         NA <NA>  source   *       
        repotype repodir     target                     deps          mirror sources  
        <chr>    <chr>       <chr>                      <list>        <chr>  <list>   
      1 <NA>     src/contrib src/contrib/pkg1_NA.tar.gz <tbl [0 x 5]> <NA>   <chr [1]>
        remote         error      metadata   extra      dep_types  params    sysreqs
        <list>         <list>     <list>     <list>     <list>     <list>    <chr>  
      1 <rmt_rf_s [6]> <async_rj> <list [0]> <list [0]> <list [0]> <chr [0]> <NA>   
        cache_status
        <chr>       
      1 miss        
      + sources:
      NA
      + remote:
      <remote_ref_standard/remote_ref/list> package: pkg1; atleast: ; version: 0.9.0; ref: pkg1@0.9.0; type: standard; params: 
      + error:
      <async_rejected/rlib_error_3_0/rlib_error/error/condition> ! pkgdepends resolution error for pkg1@0.9.0.
      Caused by error:
      ! Versioned CRAN packages are not implemented yet.
      i This feature is tracked at <https://github.com/r-lib/pak/issues/122>.
      + metadata:
      -
      + dep_types:

---

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 1 x 30
        ref        type     direct directpkg status package version license
        <chr>      <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 pkg1@1.0.0 standard TRUE   TRUE      FAILED pkg1    <NA>    <NA>   
        needscompilation priority md5sum sha256 filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>     <int> <chr> <chr>    <chr>   
      1 TRUE             <NA>     <NA>   <NA>         NA <NA>  source   *       
        repotype repodir     target                     deps          mirror sources  
        <chr>    <chr>       <chr>                      <list>        <chr>  <list>   
      1 <NA>     src/contrib src/contrib/pkg1_NA.tar.gz <tbl [0 x 5]> <NA>   <chr [1]>
        remote         error      metadata   extra      dep_types  params    sysreqs
        <list>         <list>     <list>     <list>     <list>     <list>    <chr>  
      1 <rmt_rf_s [6]> <async_rj> <list [0]> <list [0]> <list [0]> <chr [0]> <NA>   
        cache_status
        <chr>       
      1 miss        
      + sources:
      NA
      + remote:
      <remote_ref_standard/remote_ref/list> package: pkg1; atleast: ; version: 1.0.0; ref: pkg1@1.0.0; type: standard; params: 
      + error:
      <async_rejected/rlib_error_3_0/rlib_error/error/condition> ! pkgdepends resolution error for pkg1@1.0.0.
      Caused by error:
      ! Versioned CRAN packages are not implemented yet.
      i This feature is tracked at <https://github.com/r-lib/pak/issues/122>.
      + metadata:
      -
      + dep_types:

# resolve a version range

    Code
      snapshot(res, extra = "all")
    Output
      # A data frame: 1 x 30
        ref          type     direct directpkg status package version license
        <chr>        <chr>    <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>  
      1 pkg1@>=0.9.0 standard TRUE   TRUE      FAILED pkg1    <NA>    <NA>   
        needscompilation priority md5sum sha256 filesize built platform rversion
        <lgl>            <chr>    <chr>  <chr>     <int> <chr> <chr>    <chr>   
      1 TRUE             <NA>     <NA>   <NA>         NA <NA>  source   *       
        repotype repodir     target                     deps          mirror sources  
        <chr>    <chr>       <chr>                      <list>        <chr>  <list>   
      1 <NA>     src/contrib src/contrib/pkg1_NA.tar.gz <tbl [0 x 5]> <NA>   <chr [1]>
        remote         error      metadata   extra      dep_types  params    sysreqs
        <list>         <list>     <list>     <list>     <list>     <list>    <chr>  
      1 <rmt_rf_s [6]> <async_rj> <list [0]> <list [0]> <list [0]> <chr [0]> <NA>   
        cache_status
        <chr>       
      1 miss        
      + sources:
      NA
      + remote:
      <remote_ref_standard/remote_ref/list> package: pkg1; atleast: >=; version: 0.9.0; ref: pkg1@>=0.9.0; type: standard; params: 
      + error:
      <async_rejected/rlib_error_3_0/rlib_error/error/condition> ! pkgdepends resolution error for pkg1@>=0.9.0.
      Caused by error:
      ! Versioned CRAN packages are not implemented yet.
      i This feature is tracked at <https://github.com/r-lib/pak/issues/122>.
      + metadata:
      -
      + dep_types:

