# recommended_packages

    Code
      recommended_packages()
    Output
       [1] "boot"       "class"      "cluster"    "codetools"  "foreign"   
       [6] "KernSmooth" "lattice"    "MASS"       "Matrix"     "mgcv"      
      [11] "nlme"       "nnet"       "rpart"      "spatial"    "survival"  

# vlapply

    Code
      vlapply(l, identity)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 0
    Code
      vlapply(1:5, identity)
    Condition
      Error in `vapply()`:
      ! values must be type 'logical',
       but FUN(X[[1]]) result is type 'integer'

# viapply

    Code
      viapply(c(a = 1L, b = 2L), function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'integer',
       but FUN(X[[1]]) result is type 'double'

# vdapply

    Code
      vdapply(l, identity)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 0
    Code
      vdapply(letters, identity)
    Condition
      Error in `vapply()`:
      ! values must be type 'double',
       but FUN(X[[1]]) result is type 'character'

# cat0

    Code
      cat0("foo", "bar", "\n")
    Output
      foobar
    Code
      cat0("foo", "bar", "\n", sep = " ")
    Output
      foo bar 

# lapply_rows

    Code
      lapply_rows(mtcars[1:3, ], function(row) row)
    Output
      [[1]]
                mpg cyl disp  hp drat   wt  qsec vs am gear carb
      Mazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4
      
      [[2]]
                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
      
      [[3]]
                  mpg cyl disp hp drat   wt  qsec vs am gear carb
      Datsun 710 22.8   4  108 93 3.85 2.32 18.61  1  1    4    1
      

# rbind_expand

    Code
      rbind_expand(data_frame(), data_frame())
    Output
      data frame with 0 columns and 0 rows
    Code
      rbind_expand(data_frame(), data_frame(foo = 1:2))
    Output
      # A data frame: 2 x 1
          foo
        <int>
      1     1
      2     2
    Code
      rbind_expand(data_frame(bar = c("a", "b")), data_frame())
    Output
      # A data frame: 2 x 1
        bar  
        <chr>
      1 a    
      2 b    
    Code
      rbind_expand(data_frame(foo = 1:2), data_frame(foo = 3:4))
    Output
      # A data frame: 4 x 1
          foo
        <int>
      1     1
      2     2
      3     3
      4     4
    Code
      rbind_expand(data_frame(foo = 1:2), data_frame(bar = 3:4))
    Output
      # A data frame: 4 x 2
          foo   bar
        <int> <int>
      1     1    NA
      2     2    NA
      3    NA     3
      4    NA     4
    Code
      rbind_expand(data_frame(foo = list(1, 2), bar = letters[1:2]), data_frame(foo = list(
        3, 4), baz = list("x", "y")))
    Output
      # A data frame: 4 x 3
        foo       bar   baz      
        <list>    <chr> <list>   
      1 <dbl [1]> a     <lgl [1]>
      2 <dbl [1]> b     <lgl [1]>
      3 <dbl [1]> <NA>  <chr [1]>
      4 <dbl [1]> <NA>  <chr [1]>

# update_named_vector

    Code
      update_named_vector(1, c(a = 1))
    Condition
      Error:
      ! All elements in `old` must be named.
    Code
      update_named_vector(c(a = 1), 1)
    Condition
      Error:
      ! All elements in `new` must be named.

# once_per_session

    Code
      once_per_session(message("hello"))
    Message
      hello
    Code
      once_per_session(message("hello"))
      once_per_session(reset = TRUE)
      once_per_session(message("hello"))
    Message
      hello
    Code
      once_per_session(message("hello"))

# format_error_with_stdout

    Code
      format_error_with_stdout(err)
    Output
      [1] "! message (output not available)"

---

    Code
      format_error_with_stdout(err)
    Output
      [1] "! message, stdout + stderr:" ""                           
      [3] "OE> this is"                 "OE> the"                    
      [5] "OE> standard output"        

# last_stdout_lines

    Code
      last_stdout_lines(letters[1:3], "stdout + stderr")
    Output
      [1] ", stdout + stderr:" ""                   "E> a"              
      [4] "E> b"               "E> c"              

---

    Code
      last_stdout_lines(letters[1:11], "stdout + stderr")
    Output
       [1] ", stdout + stderr (last 10 lines):" ""                                  
       [3] "E> b"                               "E> c"                              
       [5] "E> d"                               "E> e"                              
       [7] "E> f"                               "E> g"                              
       [9] "E> h"                               "E> i"                              
      [11] "E> j"                               "E> k"                              

---

    Code
      last_stdout_lines(letters[1:11], "stdout + stderr")
    Output
       [1] ", stdout + stderr:" ""                   "E> a"              
       [4] "E> b"               "E> c"               "E> d"              
       [7] "E> e"               "E> f"               "E> g"              
      [10] "E> h"               "E> i"               "E> j"              
      [13] "E> k"              

# ansi_align_width [plain]

    Code
      ansi_align_width(c("foobar", cli::col_red("bar")))
    Output
      <cli_ansi_string>
      [1] foobar
      [2] bar   

# ansi_align_width [ansi]

    Code
      ansi_align_width(c("foobar", cli::col_red("bar")))
    Output
      <cli_ansi_string>
      [1] foobar                
      [2] [31mbar[39m   

