# q_library_0

    Code
      do("library(pkg, lib.loc = path)")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                       
      1     2       1     1          1       29         1            1       1         29 dep-code library(pkg, lib.loc = path)
      2     1       1     1          1        8         1            1       1          8 fn-name  library                     
    Code
      do("library('pkg', lib.loc = path)")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                          
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                         
      1     2       1     1          1       31         1            1       1         31 dep-code library('pkg', lib.loc = path)
      2     1       1     1          1        8         1            1       1          8 fn-name  library                       
    Code
      do("library(lib.loc = path, pkg)")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                       
      1     2       1     1          1       29         1            1       1         29 dep-code library(lib.loc = path, pkg)
      2     1       1     1          1        8         1            1       1          8 fn-name  library                     
    Code
      do("require(lib.loc = path, character.only = TRUE, 'pkg')")
    Output
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                                                 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                                                
      1     2       1     1          1       54         1            1       1         54 dep-code require(lib.loc = path, character.only = TRUE, 'pkg')
      2     1       1     1          1        8         1            1       1          8 fn-name  require                                              
    Code
      do("library(foo, require(bar))")
    Output
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code                      
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>                     
      1     2       1     1          1       27         1            1       1         27 dep-code library(foo, require(bar))
      2     1       1     1          1        8         1            1       1          8 fn-name  library                   
      3     2       1     2         14       26         1           14       1         26 dep-code require(bar)              
      4     1       1     2         14       21         1           14       1         21 fn-name  require                   

