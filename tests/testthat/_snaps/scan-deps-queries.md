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
       1     2       1     1         12       22         2            3       2         14 dep-code "import(\"A\")"           
       2     1       1     1         12       17         2            3       2          9 fn-name  "import"                  
       3     2       1     2         26       34         3            3       3         12 dep-code "import(B)"               
       4     1       1     2         26       31         3            3       3          9 fn-name  "import"                  
       5     2       1     3         38       55         4            3       4         21 dep-code "import(from = \"C\")"    
       6     1       1     3         38       43         4            3       4          9 fn-name  "import"                  
       7     2       1     4         59       82         5            3       5         27 dep-code "import(symbol, from = D)"
       8     1       1     4         59       64         5            3       5          9 fn-name  "import"                  
       9     2       1     5        167      177        10            1      10         12 dep-code "import(\"e\")"           
      10     1       1     5        167      172        10            1      10          7 fn-name  "import"                  
      11     2       1     6        179      187        11            1      11         10 dep-code "import(f)"               
      12     1       1     6        179      184        11            1      11          7 fn-name  "import"                  
      13     2       2     7        267      286        15            1      15         21 dep-code "modules::import(\"G\")"  
      14     3       2     7        267      273        15            1      15          8 ns-name  "modules"                 
      15     1       2     7        276      281        15           10      15         16 fn-name  "import"                  
      16     2       2     8        288      305        16            1      16         19 dep-code "modules::import(H)"      
      17     3       2     8        288      294        16            1      16          8 ns-name  "modules"                 
      18     1       2     8        297      302        16           10      16         16 fn-name  "import"                  
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
      code_query(readLines(test_path("fixtures/scan/chunk-errors.Rmd")), query = q_deps_rmd(), language = "markdown")[["matched_captures"]]
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
      14     3       1     6        217      245        20            1      20         30 content  "we forgot to close this chunk"                                                           

---

    Code
      code_query(readLines(test_path("fixtures/scan/inline-chunks.Rmd")), query = q_deps_rmd(), language = "markdown")[["matched_captures"]]
    Output
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name   code                                                                                                                           
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>  <chr>                                                                                                                          
      1     4       2     1          5       17         2            4       2         17 inline Inline Chunks                                                                                                                  
      2     4       2     2         20      146         4            1       4        128 inline Users might request the use of packages with `r inline::chunks()`. Check that we handle `r multiple::calls()` on the same line.
      3     4       2     3        149      179         6            1       6         32 inline Also in `r separate::chunks()`.                                                                                                

# q_deps_rmd_inline

    Code
      code_query(readLines(test_path("fixtures/scan/inline-chunks.Rmd")), query = q_deps_rmd_inline(), language = "markdown-inline", ranges = code[, range_cols])[["matched_captures"]]
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

# q_deps_yaml_header

    Code
      print(n = Inf, code_query(readLines(test_path("fixtures/scan/header-shiny.Rmd")), query = q_deps_yaml_header(), language = "yaml")[["matched_captures"]])
    Output
      # A data frame: 12 x 11
            id pattern match start_byte end_byte start_row start_column end_row end_column name  code                   
         <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>                  
       1     3       1     1         16       30         4            1       4         16 code  "'server': shiny"      
       2     1       1     1         16       23         4            1       4          9 key   "'server'"             
       3     2       1     1         26       30         4           11       4         16 value "shiny"                
       4     3       1     2         32       46         5            1       5         16 code  "server: 'shiny'"      
       5     1       1     2         32       37         5            1       5          7 key   "server"               
       6     2       1     2         40       46         5            9       5         16 value "'shiny'"              
       7     3       1     3         48       64         6            1       6         18 code  "\"server\": \"shiny\""
       8     1       1     3         48       55         6            1       6          9 key   "\"server\""           
       9     2       1     3         58       64         6           11       6         18 value "\"shiny\""            
      10     3       1     4         66       82         7            1       8          8 code  "server: |\n  shiny"   
      11     1       1     4         66       71         7            1       7          7 key   "server"               
      12     2       1     4         74       82         7            9       8          8 value "|\n  shiny"           
    Code
      print(n = Inf, code_query(readLines(test_path("fixtures/scan/header-shiny2.Rmd")), query = q_deps_yaml_header(), language = "yaml")[["matched_captures"]])
    Output
      # A data frame: 16 x 11
            id pattern match start_byte end_byte start_row start_column end_row end_column name  code                           
         <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>                          
       1     3       2     1         16       36         4            1       5         14 code  "server:\n  type: shiny"       
       2     1       2     1         16       21         4            1       4          7 key   "server"                       
       3     4       2     1         26       29         5            3       5          7 key2  "type"                         
       4     2       2     1         32       36         5            9       5         14 value "shiny"                        
       5     3       2     2         38       62         6            1       7         16 code  "'server':\n  type: 'shiny'"   
       6     1       2     2         38       45         6            1       6          9 key   "'server'"                     
       7     4       2     2         50       53         7            3       7          7 key2  "type"                         
       8     2       2     2         56       62         7            9       7         16 value "'shiny'"                      
       9     3       2     3         64       88         8            1       9         18 code  "server:\n  'type': \"shiny\"" 
      10     1       2     3         64       69         8            1       8          7 key   "server"                       
      11     4       2     3         74       79         9            3       9          9 key2  "'type'"                       
      12     2       2     3         82       88         9           11       9         18 value "\"shiny\""                    
      13     3       2     4         90      116        10            1      12         10 code  "server:\n  type: >\n    shiny"
      14     1       2     4         90       95        10            1      10          7 key   "server"                       
      15     4       2     4        100      103        11            3      11          7 key2  "type"                         
      16     2       2     4        106      116        11            9      12         10 value ">\n    shiny"                 

