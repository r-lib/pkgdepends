# pkgdepends HOWTO

## Dependencies

### How to list all dependencies of a CRAN/Bioconductor package?

``` r
library(pkgdepends)
prop <- new_pkg_deps("ggplot2")
prop$solve()
prop$get_solution()$data
```

    #> ✔ Loading metadata database ... done                                            
    #> # A data frame: 17 × 38                                                         
    #>    ref    type  direct directpkg status package version license needscompilation
    #>    <chr>  <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>   <lgl>           
    #>  1 cli    stan… FALSE  FALSE     OK     cli     3.6.5   MIT + … TRUE            
    #>  2 cpp11  stan… FALSE  FALSE     OK     cpp11   0.5.2   MIT + … FALSE           
    #>  3 farver stan… FALSE  FALSE     OK     farver  2.1.2   MIT + … TRUE            
    #>  4 ggplo… stan… TRUE   TRUE      OK     ggplot2 4.0.1   MIT + … FALSE           
    #>  5 glue   stan… FALSE  FALSE     OK     glue    1.8.0   MIT + … TRUE            
    #>  6 gtable stan… FALSE  FALSE     OK     gtable  0.3.6   MIT + … FALSE           
    #>  7 isoba… stan… FALSE  FALSE     OK     isoband 0.3.0   MIT + … TRUE            
    #>  8 label… stan… FALSE  FALSE     OK     labeli… 0.4.3   MIT + … FALSE           
    #>  9 lifec… stan… FALSE  FALSE     OK     lifecy… 1.0.4   MIT + … FALSE           
    #> 10 R6     stan… FALSE  FALSE     OK     R6      2.6.1   MIT + … FALSE           
    #> 11 RColo… stan… FALSE  FALSE     OK     RColor… 1.1-3   Apache… FALSE           
    #> 12 rlang  stan… FALSE  FALSE     OK     rlang   1.1.6   MIT + … TRUE            
    #> 13 S7     stan… FALSE  FALSE     OK     S7      0.2.1   MIT + … TRUE            
    #> 14 scales stan… FALSE  FALSE     OK     scales  1.4.0   MIT + … FALSE           
    #> 15 vctrs  stan… FALSE  FALSE     OK     vctrs   0.6.5   MIT + … TRUE            
    #> 16 virid… stan… FALSE  FALSE     OK     viridi… 0.4.2   MIT + … FALSE           
    #> 17 withr  stan… FALSE  FALSE     OK     withr   3.0.2   MIT + … FALSE           
    #> # ℹ 29 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,              
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,                
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>, mirror <chr>,     
    #> #   sources <list>, remote <list>, error <list>, metadata <list>, extra <list>, 
    #> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>,              
    #> #   cache_status <chr>, sysreqs_packages <list>, sysreqs_pre_install <chr>,     
    #> #   sysreqs_post_install <chr>, sysreqs_install <chr>, lib_status <chr>, …      

You can also draw a dependency tree:

``` r
prop$draw()
```

    #> ggplot2 4.0.1 [new][bld][dl] (6.34 MB)                                          
    #> ├─cli 3.6.5 [new][bld][cmp]                                                     
    #> ├─gtable 0.3.6 [new][bld][dl] (148.15 kB)                                       
    #> │ ├─cli                                                                         
    #> │ ├─glue 1.8.0 [new][bld][cmp]                                                  
    #> │ ├─lifecycle 1.0.4 [new][bld]                                                  
    #> │ │ ├─cli                                                                       
    #> │ │ ├─glue                                                                      
    #> │ │ └─rlang 1.1.6 [new][bld][cmp]                                               
    #> │ └─rlang                                                                       
    #> ├─isoband 0.3.0 [new][bld][cmp][dl] (1.59 MB)                                   
    #> │ ├─cli                                                                         
    #> │ ├─cpp11 0.5.2 [new][bld][dl] (291.99 kB)                                      
    #> │ └─rlang                                                                       
    #> ├─lifecycle                                                                     
    #> ├─rlang                                                                         
    #> ├─S7 0.2.1 [new][bld][cmp][dl] (184.14 kB)                                      
    #> ├─scales 1.4.0 [new][bld][dl] (328.67 kB)                                       
    #> │ ├─cli                                                                         
    #> │ ├─farver 2.1.2 [new][bld][cmp][dl] (1.28 MB)                                  
    #> │ ├─glue                                                                        
    #> │ ├─labeling 0.4.3 [new][bld][dl] (10.17 kB)                                    
    #> │ ├─lifecycle                                                                   
    #> │ ├─R6 2.6.1 [new][bld]                                                         
    #> │ ├─RColorBrewer 1.1-3 [new][bld][dl] (11.64 kB)                                
    #> │ ├─rlang                                                                       
    #> │ └─viridisLite 0.4.2 [new][bld][dl] (1.27 MB)                                  
    #> ├─vctrs 0.6.5 [new][bld][cmp][dl] (969.07 kB)                                   
    #> │ ├─cli                                                                         
    #> │ ├─glue                                                                        
    #> │ ├─lifecycle                                                                   
    #> │ └─rlang                                                                       
    #> └─withr 3.0.2 [new][bld][dl] (103.24 kB)                                        
    #>                                                                                 
    #> Key:  [new] new | [dl] download | [bld] build | [cmp] compile                   

### How to list all dependencies of a GitHub package?

``` r
library(pkgdepends)
prop <- new_pkg_deps("tidyverse/ggplot2")
prop$solve()
prop$get_solution()$data
```

    #> # A data frame: 17 × 38                                                         
    #>    ref    type  direct directpkg status package version license needscompilation
    #>    <chr>  <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>   <lgl>           
    #>  1 tidyv… gith… TRUE   TRUE      OK     ggplot2 4.0.1.… MIT + … TRUE            
    #>  2 cli    stan… FALSE  FALSE     OK     cli     3.6.5   MIT + … TRUE            
    #>  3 cpp11  stan… FALSE  FALSE     OK     cpp11   0.5.2   MIT + … FALSE           
    #>  4 farver stan… FALSE  FALSE     OK     farver  2.1.2   MIT + … TRUE            
    #>  5 glue   stan… FALSE  FALSE     OK     glue    1.8.0   MIT + … TRUE            
    #>  6 gtable stan… FALSE  FALSE     OK     gtable  0.3.6   MIT + … FALSE           
    #>  7 isoba… stan… FALSE  FALSE     OK     isoband 0.3.0   MIT + … TRUE            
    #>  8 label… stan… FALSE  FALSE     OK     labeli… 0.4.3   MIT + … FALSE           
    #>  9 lifec… stan… FALSE  FALSE     OK     lifecy… 1.0.4   MIT + … FALSE           
    #> 10 R6     stan… FALSE  FALSE     OK     R6      2.6.1   MIT + … FALSE           
    #> 11 RColo… stan… FALSE  FALSE     OK     RColor… 1.1-3   Apache… FALSE           
    #> 12 rlang  stan… FALSE  FALSE     OK     rlang   1.1.6   MIT + … TRUE            
    #> 13 S7     stan… FALSE  FALSE     OK     S7      0.2.1   MIT + … TRUE            
    #> 14 scales stan… FALSE  FALSE     OK     scales  1.4.0   MIT + … FALSE           
    #> 15 vctrs  stan… FALSE  FALSE     OK     vctrs   0.6.5   MIT + … TRUE            
    #> 16 virid… stan… FALSE  FALSE     OK     viridi… 0.4.2   MIT + … FALSE           
    #> 17 withr  stan… FALSE  FALSE     OK     withr   3.0.2   MIT + … FALSE           
    #> # ℹ 29 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,              
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,                
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>, mirror <chr>,     
    #> #   sources <list>, remote <list>, error <list>, metadata <list>, extra <list>, 
    #> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>,              
    #> #   cache_status <chr>, sysreqs_packages <list>, sysreqs_pre_install <chr>,     
    #> #   sysreqs_post_install <chr>, sysreqs_install <chr>, lib_status <chr>, …      

### How to list all dependencies of a local package?

``` r
library(pkgdepends)
prop <- new_pkg_deps("local::.")
prop$solve()
prop$get_solution()$data
```

    #> # A data frame: 14 × 38                                                         
    #>    ref    type  direct directpkg status package version license needscompilation
    #>    <chr>  <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>   <lgl>           
    #>  1 local… local TRUE   TRUE      OK     pkgdep… 0.9.0.… MIT + … NA              
    #>  2 callr  stan… FALSE  FALSE     OK     callr   3.7.6   MIT + … FALSE           
    #>  3 cli    stan… FALSE  FALSE     OK     cli     3.6.5   MIT + … TRUE            
    #>  4 curl   stan… FALSE  FALSE     OK     curl    7.0.0   MIT + … TRUE            
    #>  5 desc   stan… FALSE  FALSE     OK     desc    1.4.3   MIT + … FALSE           
    #>  6 filel… stan… FALSE  FALSE     OK     filelo… 1.0.3   MIT + … TRUE            
    #>  7 jsonl… stan… FALSE  FALSE     OK     jsonli… 2.0.0   MIT + … TRUE            
    #>  8 lpSol… stan… FALSE  FALSE     OK     lpSolve 5.6.23  LGPL-2  TRUE            
    #>  9 pkgbu… stan… FALSE  FALSE     OK     pkgbui… 1.4.8   MIT + … FALSE           
    #> 10 pkgca… stan… FALSE  FALSE     OK     pkgcac… 2.2.4   MIT + … TRUE            
    #> 11 proce… stan… FALSE  FALSE     OK     proces… 3.8.6   MIT + … TRUE            
    #> 12 ps     stan… FALSE  FALSE     OK     ps      1.9.1   MIT + … TRUE            
    #> 13 R6     stan… FALSE  FALSE     OK     R6      2.6.1   MIT + … FALSE           
    #> 14 zip    stan… FALSE  FALSE     OK     zip     2.3.3   MIT + … TRUE            
    #> # ℹ 29 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,              
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,                
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>, mirror <chr>,     
    #> #   sources <list>, remote <list>, error <list>, metadata <list>, extra <list>, 
    #> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>,              
    #> #   cache_status <chr>, sysreqs_packages <list>, sysreqs_pre_install <chr>,     
    #> #   sysreqs_post_install <chr>, sysreqs_install <chr>, lib_status <chr>, …      

## Downloads

### How to download a package and all of its dependencies?

``` r
library(pkgdepends)
target_dir <- tempfile()
dir.create(target_dir)
prop <- new_pkg_download_proposal("ggplot2", config = list(cache_dir = target_dir))
prop$resolve()
prop$download()
prop$get_downloads()
dir(target_dir)
```

    #> ℹ Getting 12 pkgs (12.53 MB), 5 (1.71 MB) cached                                
    #> ✔ Got cpp11 0.5.2 (source) (291.99 kB)                                          
    #> ✔ Got farver 2.1.2 (source) (1.28 MB)                                           
    #> ✔ Got RColorBrewer 1.1-3 (source) (11.64 kB)                                    
    #> ✔ Got S7 0.2.1 (source) (184.14 kB)                                             
    #> ✔ Got scales 1.4.0 (source) (328.67 kB)                                         
    #> ✔ Got gtable 0.3.6 (source) (148.15 kB)                                         
    #> ✔ Got withr 3.0.2 (source) (103.24 kB)                                          
    #> ✔ Got isoband 0.3.0 (source) (1.59 MB)                                          
    #> ✔ Got labeling 0.4.3 (source) (10.17 kB)                                        
    #> ✔ Got ggplot2 4.0.1 (source) (6.34 MB)                                          
    #> ✔ Got vctrs 0.6.5 (source) (969.07 kB)                                          
    #> ✔ Got viridisLite 0.4.2 (source) (1.27 MB)                                      
    #> # A data frame: 17 × 41                                                         
    #>    ref    type  direct directpkg status package version license needscompilation
    #>    <chr>  <chr> <lgl>  <lgl>     <chr>  <chr>   <chr>   <chr>   <lgl>           
    #>  1 cli    stan… FALSE  FALSE     OK     cli     3.6.5   MIT + … TRUE            
    #>  2 cpp11  stan… FALSE  FALSE     OK     cpp11   0.5.2   MIT + … FALSE           
    #>  3 farver stan… FALSE  FALSE     OK     farver  2.1.2   MIT + … TRUE            
    #>  4 ggplo… stan… TRUE   TRUE      OK     ggplot2 4.0.1   MIT + … FALSE           
    #>  5 glue   stan… FALSE  FALSE     OK     glue    1.8.0   MIT + … TRUE            
    #>  6 gtable stan… FALSE  FALSE     OK     gtable  0.3.6   MIT + … FALSE           
    #>  7 isoba… stan… FALSE  FALSE     OK     isoband 0.3.0   MIT + … TRUE            
    #>  8 label… stan… FALSE  FALSE     OK     labeli… 0.4.3   MIT + … FALSE           
    #>  9 lifec… stan… FALSE  FALSE     OK     lifecy… 1.0.4   MIT + … FALSE           
    #> 10 R6     stan… FALSE  FALSE     OK     R6      2.6.1   MIT + … FALSE           
    #> 11 RColo… stan… FALSE  FALSE     OK     RColor… 1.1-3   Apache… FALSE           
    #> 12 rlang  stan… FALSE  FALSE     OK     rlang   1.1.6   MIT + … TRUE            
    #> 13 S7     stan… FALSE  FALSE     OK     S7      0.2.1   MIT + … TRUE            
    #> 14 scales stan… FALSE  FALSE     OK     scales  1.4.0   MIT + … FALSE           
    #> 15 vctrs  stan… FALSE  FALSE     OK     vctrs   0.6.5   MIT + … TRUE            
    #> 16 virid… stan… FALSE  FALSE     OK     viridi… 0.4.2   MIT + … FALSE           
    #> 17 withr  stan… FALSE  FALSE     OK     withr   3.0.2   MIT + … FALSE           
    #> # ℹ 32 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,              
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,                
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>, mirror <chr>,     
    #> #   sources <list>, remote <list>, error <list>, metadata <list>, extra <list>, 
    #> #   dep_types <list>, params <list>, sysreqs <chr>, os_type <chr>,              
    #> #   cache_status <chr>, sysreqs_packages <list>, sysreqs_pre_install <chr>,     
    #> #   sysreqs_post_install <chr>, sysreqs_install <chr>, fulltarget <chr>, …      
    #> [1] "src"                                                                       

## Installation

### How to install a package into a new library?

``` r
library(pkgdepends)
dir.create(new_lib <- tempfile())
prop <- new_pkg_installation_proposal("pkgconfig", config = list(library = new_lib))
prop$solve()
prop$download()
prop$install()
lib_status(new_lib)
```

    #> ℹ Getting 1 pkg (6.08 kB)                                                       
    #> ✔ Got pkgconfig 2.0.3 (source) (6.08 kB)                                        
    #> ℹ Building pkgconfig 2.0.3                                                      
    #> ✔ Built pkgconfig 2.0.3 (848ms)                                                 
    #> ✔ Installed pkgconfig 2.0.3  (1s)                                               
    #> ✔ Summary:   1 new  in 1.9s                                                     
    #> # A data frame: 1 × 31                                                          
    #>   library        package title version license imports suggests needscompilation
    #>   <chr>          <chr>   <chr> <chr>   <chr>   <chr>   <chr>    <lgl>           
    #> 1 /tmp/RtmpPDSo… pkgcon… Priv… 2.0.3   MIT + … utils   covr, t… FALSE           
    #> # ℹ 23 more variables: repository <chr>, built <chr>, remotetype <chr>,         
    #> #   remotepkgref <chr>, remoteref <chr>, remoterepos <chr>,                     
    #> #   remotepkgplatform <chr>, remotesha <chr>, depends <chr>, linkingto <chr>,   
    #> #   enhances <chr>, md5sum <chr>, platform <chr>, priority <chr>,               
    #> #   biocviews <chr>, sysreqs <chr>, ref <chr>, type <chr>, status <chr>,        
    #> #   rversion <chr>, sources <list>, repotype <chr>, deps <list>                 

### How to update a package?

Install an older version first.

``` r
library(pkgdepends)
dir.create(new_lib <- tempfile())
config <- list(library = new_lib)
prop <- new_pkg_installation_proposal("cran/pkgconfig@2.0.2", config = config)
prop$solve()
prop$download()
prop$install()
lib_status(new_lib)
```

    #> ℹ No downloads are needed, 1 pkg is cached                                      
    #> ✔ Got pkgconfig 2.0.2 (source) (13.22 kB)                                       
    #> ℹ Packaging pkgconfig 2.0.2                                                     
    #> ✔ Packaged pkgconfig 2.0.2 (483ms)                                              
    #> ℹ Building pkgconfig 2.0.2                                                      
    #> ✔ Built pkgconfig 2.0.2 (855ms)                                                 
    #> ✔ Installed pkgconfig 2.0.2 (github::cran/pkgconfig@d892880) (1s)               
    #> ✔ Summary:   1 new  in 1.9s                                                     
    #> # A data frame: 1 × 32                                                          
    #>   library        package title version license imports suggests needscompilation
    #>   <chr>          <chr>   <chr> <chr>   <chr>   <chr>   <chr>    <lgl>           
    #> 1 /tmp/RtmpPDSo… pkgcon… Priv… 2.0.2   MIT + … utils   covr, t… FALSE           
    #> # ℹ 24 more variables: repository <chr>, remotetype <chr>, remotehost <chr>,    
    #> #   remoterepo <chr>, remoteusername <chr>, remotepkgref <chr>,                 
    #> #   remoteref <chr>, remotesha <chr>, built <chr>, depends <chr>,               
    #> #   linkingto <chr>, enhances <chr>, md5sum <chr>, platform <chr>,              
    #> #   priority <chr>, biocviews <chr>, sysreqs <chr>, ref <chr>, type <chr>,      
    #> #   status <chr>, rversion <chr>, sources <list>, repotype <chr>, deps <list>   

Now update.

``` r
library(pkgdepends)
prop2 <- new_pkg_installation_proposal("pkgconfig", config = config)
prop2$set_solve_policy("upgrade")
prop2$solve()
prop2$download()
prop2$install()
lib_status(new_lib)
```

    #> ℹ No downloads are needed, 1 pkg (6.08 kB) is cached                            
    #> ✔ Installed pkgconfig 2.0.3  (1s)                                               
    #> ✔ Summary:   1 updated  in 1s                                                   
    #> # A data frame: 1 × 31                                                          
    #>   library        package title version license imports suggests needscompilation
    #>   <chr>          <chr>   <chr> <chr>   <chr>   <chr>   <chr>    <lgl>           
    #> 1 /tmp/RtmpPDSo… pkgcon… Priv… 2.0.3   MIT + … utils   covr, t… FALSE           
    #> # ℹ 23 more variables: repository <chr>, built <chr>, remotetype <chr>,         
    #> #   remotepkgref <chr>, remoteref <chr>, remoterepos <chr>,                     
    #> #   remotepkgplatform <chr>, remotesha <chr>, depends <chr>, linkingto <chr>,   
    #> #   enhances <chr>, md5sum <chr>, platform <chr>, priority <chr>,               
    #> #   biocviews <chr>, sysreqs <chr>, ref <chr>, type <chr>, status <chr>,        
    #> #   rversion <chr>, sources <list>, repotype <chr>, deps <list>                 
