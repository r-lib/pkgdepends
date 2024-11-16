# q_library_0

    Code
      do("library(pkg, lib.loc = path)")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                       
      1     2       1     1          1       28         1            1       1         29 dep-code library(pkg, lib.loc = path)
      2     1       1     1          1        7         1            1       1          8 fn-name  library                     
    Code
      do("library('pkg', lib.loc = path)")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                          
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                         
      1     2       1     1          1       30         1            1       1         31 dep-code library('pkg', lib.loc = path)
      2     1       1     1          1        7         1            1       1          8 fn-name  library                       
    Code
      do("library(lib.loc = path, pkg)")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                       
      1     2       1     1          1       28         1            1       1         29 dep-code library(lib.loc = path, pkg)
      2     1       1     1          1        7         1            1       1          8 fn-name  library                     
    Code
      do("require(lib.loc = path, character.only = TRUE, 'pkg')")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                                                 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                                                
      1     2       1     1          1       53         1            1       1         54 dep-code require(lib.loc = path, character.only = TRUE, 'pkg')
      2     1       1     1          1        7         1            1       1          8 fn-name  require                                              
    Code
      do("library(foo, require(bar))")
    Output
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                      
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                     
      1     2       1     1          1       26         1            1       1         27 dep-code library(foo, require(bar))
      2     1       1     1          1        7         1            1       1          8 fn-name  library                   
      3     2       1     2         14       25         1           14       1         26 dep-code require(bar)              
      4     1       1     2         14       20         1           14       1         21 fn-name  require                   

# q_module_import

    Code
      do("fixtures/scan/modules.R")
    Output
      # A data frame: 18 x 11
            id pattern match start_byte end_byte start_row start_column end_row end_column name     code                      
         <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                     
       1     2       1     1         11       21         2            2       2         13 dep-code "import(\"A\")"           
       2     1       1     1         11       16         2            2       2          8 fn-name  "import"                  
       3     2       1     2         24       32         3            2       3         11 dep-code "import(B)"               
       4     1       1     2         24       29         3            2       3          8 fn-name  "import"                  
       5     2       1     3         35       52         4            2       4         20 dep-code "import(from = \"C\")"    
       6     1       1     3         35       40         4            2       4          8 fn-name  "import"                  
       7     2       1     4         55       78         5            2       5         26 dep-code "import(symbol, from = D)"
       8     1       1     4         55       60         5            2       5          8 fn-name  "import"                  
       9     2       1     5        163      173        10            1      10         12 dep-code "import(\"e\")"           
      10     1       1     5        163      168        10            1      10          7 fn-name  "import"                  
      11     2       1     6        175      183        11            1      11         10 dep-code "import(f)"               
      12     1       1     6        175      180        11            1      11          7 fn-name  "import"                  
      13     2       2     7        263      282        15            1      15         21 dep-code "modules::import(\"G\")"  
      14     3       2     7        263      269        15            1      15          8 ns-name  "modules"                 
      15     1       2     7        272      277        15           10      15         16 fn-name  "import"                  
      16     2       2     8        284      301        16            1      16         19 dep-code "modules::import(H)"      
      17     3       2     8        284      290        16            1      16          8 ns-name  "modules"                 
      18     1       2     8        293      298        16           10      16         16 fn-name  "import"                  
    Code
      do("fixtures/scan/modules-empty.R")
    Output
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>

# q_colon

    Code
      do("x <- foo::bar()")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code    
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>   
      1     2       1     1          6       13         1            6       1         14 dep-code foo::bar
      2     1       1     1          6        8         1            6       1          9 pkg-name foo     
    Code
      do("1 + 2 + foo:::bar")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code     
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>    
      1     2       1     1          9       17         1            9       1         18 dep-code foo:::bar
      2     1       1     1          9       11         1            9       1         12 pkg-name foo      

# q_methods

    Code
      do("setClass('myclass')")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code               
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>              
      1     2       1     1          1       19         1            1       1         20 dep-code setClass('myclass')
      2     1       1     1          1        8         1            1       1          9 fn-name  setClass           
    Code
      do("setGeneric('props', function(object) attributes(object))")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                                                    
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                                                   
      1     2       1     1          1       56         1            1       1         57 dep-code setGeneric('props', function(object) attributes(object))
      2     1       1     1          1       10         1            1       1         11 fn-name  setGeneric                                              

# q_junit_reporter

    Code
      do("JunitReporter$new()")
    Output
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name        code               
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>       <chr>              
      1     3       1     1          1       19         1            1       1         20 dep-code    JunitReporter$new()
      2     1       1     1          1       13         1            1       1         14 class-name  JunitReporter      
      3     2       1     1         15       17         1           15       1         18 method-name new                
    Code
      do("testthat::JunitReporter$new()")
    Output
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name        code                         
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>       <chr>                        
      1     3       2     1          1       29         1            1       1         30 dep-code    testthat::JunitReporter$new()
      2     4       2     1          1        8         1            1       1          9 pkg-name    testthat                     
      3     1       2     1         11       23         1           11       1         24 class-name  JunitReporter                
      4     2       2     1         25       27         1           25       1         28 method-name new                          

# q_knitr_dev

    Code
      do("opts_chunk$set()")
    Output
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name        code            
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>       <chr>           
      1     3       1     1          1       16         1            1       1         17 dep-code    opts_chunk$set()
      2     1       1     1          1       10         1            1       1         11 object-name opts_chunk      
      3     2       1     1         12       14         1           12       1         15 method-name set             
    Code
      do("knitr::opts_chunk$set()")
    Output
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name        code                   
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>       <chr>                  
      1     3       2     1          1       23         1            1       1         24 dep-code    knitr::opts_chunk$set()
      2     4       2     1          1        5         1            1       1          6 pkg-name    knitr                  
      3     1       2     1          8       17         1            8       1         18 object-name opts_chunk             
      4     2       2     1         19       21         1           19       1         22 method-name set                    

# renv_dependencies_database

    Code
      renv_dependencies_database()
    Output
      $ggplot2
      $ggplot2$geom_hex
      [1] "hexbin"
      
      
      $testthat
      $testthat$JunitReporter
      [1] "xml2"
      
      

# q_database

    Code
      do("geom_hex()")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code    
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>   
      1     1       1     1          1        8         1            1       1          9 id    geom_hex
    Code
      do("ggplot2::geom_hex()")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code    
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>   
      1     1       1     1         10       17         1           10       1         18 id    geom_hex
    Code
      do("JunitReporter")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code         
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>        
      1     1       1     1          1       13         1            1       1         14 id    JunitReporter
    Code
      do("testthat::JunitReporter")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code         
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>        
      1     1       1     1         11       23         1           11       1         24 id    JunitReporter

# q_database #3

    Code
      do("geom_hex()")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code    
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>   
      1     1       1     1          1        8         1            1       1          9 id    geom_hex
    Code
      do("foopkg::foofun()")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code  
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr> 
      1     1       1     1          9       14         1            9       1         15 id    foofun
    Code
      do("foofun")
    Output
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code  
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr> 
      1     1       1     1          1        6         1            1       1          7 id    foofun

# q_deps

    Code
      q_deps()
    Output
      [1] 1 2 3 4 5 6

# q_deps_rmd

    Code
      code_query(file = test_path("fixtures/scan/chunk-errors.Rmd"), query = q_deps_rmd(), language = "markdown")[["matched_captures"]]
    Output
      # A data frame: 14 x 11
            id pattern match start_byte end_byte start_row start_column end_row end_column name     code                                                                                      
         <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                                                                                     
       1     4       2     1          5       16         2            4       2         16 inline   "Parse Errors"                                                                            
       2     4       2     2         19      105         4            1       5         14 inline   "renv should still be able to recover dependencies from the chunks without\nparse errors."
       3     2       1     3        111      113         7            4       7          7 header   "{r}"                                                                                     
       4     1       1     3        112      112         7            5       7          6 language "r"                                                                                       
       5     3       1     3        115      129         8            1       9          1 content  "library(dplyr)\n"                                                                        
       6     2       1     4        138      140        11            4      11          7 header   "{r}"                                                                                     
       7     1       1     4        139      139        11            5      11          6 language "r"                                                                                       
       8     3       1     4        142      169        12            1      13          1 content  "this chunk has parse errors\n"                                                           
       9     2       1     5        178      180        15            4      15          7 header   "{r}"                                                                                     
      10     1       1     5        179      179        15            5      15          6 language "r"                                                                                       
      11     3       1     5        182      204        16            1      17          1 content  "and so does this chunk\n"                                                                
      12     2       1     6        213      215        19            4      19          7 header   "{r}"                                                                                     
      13     1       1     6        214      214        19            5      19          6 language "r"                                                                                       
      14     3       1     6        217      246        20            1      21          1 content  "we forgot to close this chunk\n"                                                         

---

    Code
      code_query(file = test_path("fixtures/scan/inline-chunks.Rmd"), query = q_deps_rmd(), language = "markdown")[["matched_captures"]]
    Output
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name   code                                                                                                                           
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>  <chr>                                                                                                                          
      1     4       2     1          5       17         2            4       2         17 inline Inline Chunks                                                                                                                  
      2     4       2     2         20      146         4            1       4        128 inline Users might request the use of packages with `r inline::chunks()`. Check that we handle `r multiple::calls()` on the same line.
      3     4       2     3        149      179         6            1       6         32 inline Also in `r separate::chunks()`.                                                                                                

# q_deps_rmd_inline

    Code
      code_query(file = test_path("fixtures/scan/inline-chunks.Rmd"), query = q_deps_rmd_inline(), language = "markdown-inline", ranges = code[, range_cols])[["matched_captures"]]
    Output
      # A data frame: 9 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code                  
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>                 
      1     3       1     1         65       84         4           46       4         66 code  `r inline::chunks()`  
      2     1       1     1         65       65         4           46       4         47 csd1  `                     
      3     2       1     1         84       84         4           65       4         66 csd2  `                     
      4     3       1     2        108      128         4           89       4        110 code  `r multiple::calls()` 
      5     1       1     2        108      108         4           89       4         90 csd1  `                     
      6     2       1     2        128      128         4          109       4        110 csd2  `                     
      7     3       1     3        157      178         6            9       6         31 code  `r separate::chunks()`
      8     1       1     3        157      157         6            9       6         10 csd1  `                     
      9     2       1     3        178      178         6           30       6         31 csd2  `                     

