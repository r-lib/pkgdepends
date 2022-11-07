# binary preferred over source

    Code
      sol[, c("package", "platform")]
    Output
      # A data frame: 2 x 2
        package platform               
      * <chr>   <chr>                  
      1 pkg     i386+x86_64-w64-mingw32
      2 pkg2    i386+x86_64-w64-mingw32

# but source is used if that version is required

    Code
      sol[, c("package", "version", "platform")]
    Output
      # A data frame: 2 x 3
        package version platform               
      * <chr>   <chr>   <chr>                  
      1 pkg     1.0.0   i386+x86_64-w64-mingw32
      2 pkg2    2.0.0   source                 

