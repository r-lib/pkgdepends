# q_library_0

    Code
      do("library(pkg, lib.loc = path)")
    Output
      # A data frame: 2 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     2       1     1          1         1            1 call-code library(pkg, ~
      2     1       1     1          1         1            1 fn-name   library       
    Code
      do("library('pkg', lib.loc = path)")
    Output
      # A data frame: 2 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     2       1     1          1         1            1 call-code library('pkg'~
      2     1       1     1          1         1            1 fn-name   library       
    Code
      do("library(lib.loc = path, pkg)")
    Output
      # A data frame: 2 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     2       1     1          1         1            1 call-code library(lib.l~
      2     1       1     1          1         1            1 fn-name   library       
    Code
      do("require(lib.loc = path, character.only = TRUE, 'pkg')")
    Output
      # A data frame: 2 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     2       1     1          1         1            1 call-code require(lib.l~
      2     1       1     1          1         1            1 fn-name   require       
    Code
      do("library(foo, require(bar))")
    Output
      # A data frame: 4 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     2       1     1          1         1            1 call-code library(foo, ~
      2     1       1     1          1         1            1 fn-name   library       
      3     2       1     2         14         1           14 call-code require(bar)  
      4     1       1     2         14         1           14 fn-name   require       

# q_library_1

    Code
      code_query("library(pkg)", q_library_1())[["matched_captures"]]
    Output
      # A data frame: 3 x 8
           id pattern match start_byte start_row start_column name      code        
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>       
      1     3       1     1          1         1            1 call-code library(pkg)
      2     1       1     1          1         1            1 fn-name   library     
      3     2       1     1          9         1            9 pkg-name  pkg         
    Code
      code_query("library('pkg')", q_library_1())[["matched_captures"]]
    Output
      # A data frame: 3 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     3       1     1          1         1            1 call-code library('pkg')
      2     1       1     1          1         1            1 fn-name   library       
      3     2       1     1         10         1           10 pkg-name  pkg           
    Code
      code_query("require(pkg)", q_library_1())[["matched_captures"]]
    Output
      # A data frame: 3 x 8
           id pattern match start_byte start_row start_column name      code        
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>       
      1     3       1     1          1         1            1 call-code require(pkg)
      2     1       1     1          1         1            1 fn-name   require     
      3     2       1     1          9         1            9 pkg-name  pkg         
    Code
      code_query("require('pkg')", q_library_1())[["matched_captures"]]
    Output
      # A data frame: 3 x 8
           id pattern match start_byte start_row start_column name      code          
        <int>   <int> <int>      <int>     <int>        <int> <chr>     <chr>         
      1     3       1     1          1         1            1 call-code require('pkg')
      2     1       1     1          1         1            1 fn-name   require       
      3     2       1     1         10         1           10 pkg-name  pkg           

