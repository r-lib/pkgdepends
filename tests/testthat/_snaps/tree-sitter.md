# s_expr

    Code
      s_expr("f(arg, arg2 = 2); 1:100")
    Output
      [1] "(program (call function: (identifier) arguments: (arguments argument: (argument value: (identifier)) (comma) argument: (argument name: (identifier) value: (float)))) (binary_operator lhs: (float) rhs: (float)))"

# code_query

    Code
      code_query("f(arg, arg2)", "(call(arguments))")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern               match_count
        <int> <chr> <chr>                       <int>
      1     1 <NA>  "(call(arguments))\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      code_query("f(arg, arg2)", "(call(arguments)) @call")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                     match_count
        <int> <chr> <chr>                             <int>
      1     1 <NA>  "(call(arguments)) @call\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>       
      1     1       1     1          1       12         1            1       1         13 call  f(arg, arg2)
      

# code_query, multiple patterns

    Code
      code_query("f(arg, arg2)", "(call(arguments)) @call (arguments) @args")
    Output
      $patterns
      # A data frame: 2 x 4
           id name  pattern                    match_count
        <int> <chr> <chr>                            <int>
      1     1 <NA>  "(call(arguments)) @call "           1
      2     2 <NA>  "(arguments) @args\n"                1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>       
      1     1       1     1          1       12         1            1       1         13 call  f(arg, arg2)
      2     2       2     2          2       12         1            2       1         13 args  (arg, arg2) 
      
    Code
      code_query("f(arg, arg2)", c("(call(arguments)) @call", "(arguments) @args"))
    Output
      $patterns
      # A data frame: 2 x 4
           id name  pattern                     match_count
        <int> <chr> <chr>                             <int>
      1     1 <NA>  "(call(arguments)) @call\n"           1
      2     2 <NA>  "(arguments) @args\n"                 1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code        
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>       
      1     1       1     1          1       12         1            1       1         13 call  f(arg, arg2)
      2     2       2     2          2       12         1            2       1         13 args  (arg, arg2) 
      

# pattern names

    Code
      code_query("f('x')", c("(call) (call)", "(call)"))[["patterns"]]
    Output
      # A data frame: 3 x 4
           id name  pattern    match_count
        <int> <chr> <chr>            <int>
      1     1 <NA>  "(call) "            1
      2     2 <NA>  "(call)\n"           1
      3     3 <NA>  "(call)\n"           1
    Code
      code_query("f('x')", c(a = "(call) (call)", "(call)"))[["patterns"]]
    Output
      # A data frame: 3 x 4
           id name  pattern    match_count
        <int> <chr> <chr>            <int>
      1     1 "a"   "(call) "            1
      2     2 "a"   "(call)\n"           1
      3     3 ""    "(call)\n"           1
    Code
      code_query("f('x')", c("(call) (call)", b = "(call)"))[["patterns"]]
    Output
      # A data frame: 3 x 4
           id name  pattern    match_count
        <int> <chr> <chr>            <int>
      1     1 ""    "(call) "            1
      2     2 ""    "(call)\n"           1
      3     3 "b"   "(call)\n"           1
    Code
      code_query("f('x')", c(a = "(call) (call)", b = "(call)"))[["patterns"]]
    Output
      # A data frame: 3 x 4
           id name  pattern    match_count
        <int> <chr> <chr>            <int>
      1     1 a     "(call) "            1
      2     2 a     "(call)\n"           1
      3     3 b     "(call)\n"           1

# syntax error is handled

    Code
      code_query("f(1); g(1,2); 1+; h(3)", "(call) @call-code")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern               match_count
        <int> <chr> <chr>                       <int>
      1     1 <NA>  "(call) @call-code\n"           3
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name      code  
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>     <chr> 
      1     1       1     1          1        4         1            1       1          5 call-code f(1)  
      2     1       1     2          7       12         1            7       1         13 call-code g(1,2)
      3     1       1     3         19       22         1           19       1         23 call-code h(3)  
      

# code_query, field names

    Code
      code_query("f(arg, x = arg2)", "(argument name: (identifier) @name value: (identifier) @value)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                            match_count
        <int> <chr> <chr>                                                                    <int>
      1     1 <NA>  "(argument name: (identifier) @name value: (identifier) @value)\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          8        8         1            8       1          9 name  x    
      2     2       1     1         12       15         1           12       1         16 value arg2 
      

# code_query, negated fields

    Code
      code_query("f(arg1, x = arg2); g(); a", "((call (arguments !argument)) @call-name)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                       match_count
        <int> <chr> <chr>                                               <int>
      1     1 <NA>  "((call (arguments !argument)) @call-name)\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name      code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>     <chr>
      1     1       1     1         20       22         1           20       1         23 call-name g()  
      

# code_query, anonymous nodes

    Code
      code_query("1 + 2", "((float) @lhs \"+\" @op (float)) @rhs")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                   match_count
        <int> <chr> <chr>                                           <int>
      1     1 <NA>  "((float) @lhs \"+\" @op (float)) @rhs\n"           1
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 lhs   1    
      2     3       1     1          1        1         1            1       1          2 rhs   1    
      3     2       1     1          3        3         1            3       1          4 op    +    
      

# code_query, capturing nodes

    Code
      code_query("library(testthat); foo(bar); library(pak); bar(baz)", "(call function: (identifier) @fun-name)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                     match_count
        <int> <chr> <chr>                                             <int>
      1     1 <NA>  "(call function: (identifier) @fun-name)\n"           4
      
      $matched_captures
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code   
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>  
      1     1       1     1          1        7         1            1       1          8 fun-name library
      2     1       1     2         20       22         1           20       1         23 fun-name foo    
      3     1       1     3         30       36         1           30       1         37 fun-name library
      4     1       1     4         44       46         1           44       1         47 fun-name bar    
      

# code_query, quantification operators

    Code
      code_query("# comment\n#comment 2\n1 + 5\n#comment 3\n", "(comment)+ @comments")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                  match_count
        <int> <chr> <chr>                          <int>
      1     1 <NA>  "(comment)+ @comments\n"           2
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code      
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>     
      1     1       1     1          1        9         1            1       1         10 comments # comment 
      2     1       1     1         11       20         2            1       2         11 comments #comment 2
      3     1       1     2         28       37         4            1       4         11 comments #comment 3
      

---

    Code
      code_query("f()\n# comment\ng()# comment1\n# comment 2\nh()\n", "((comment)* @comments (call function: (identifier)) @call)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                        match_count
        <int> <chr> <chr>                                                                <int>
      1     1 <NA>  "((comment)* @comments (call function: (identifier)) @call)\n"           4
      
      $matched_captures
      # A data frame: 8 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code       
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>      
      1     2       1     1          1        3         1            1       1          4 call     f()        
      2     1       1     2          5       13         2            1       2         10 comments # comment  
      3     2       1     2         15       17         3            1       3          4 call     g()        
      4     1       1     3          5       13         2            1       2         10 comments # comment  
      5     2       1     3         41       43         5            1       5          4 call     h()        
      6     1       1     4         18       27         3            4       3         14 comments # comment1 
      7     1       1     4         29       39         4            1       4         12 comments # comment 2
      8     2       1     4         41       43         5            1       5          4 call     h()        
      

---

    Code
      code_query("f()\n# comment\ng()# comment1\n# comment 2\nh()\n", "((comment)? @comment (call function: (identifier)) @call)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                       match_count
        <int> <chr> <chr>                                                               <int>
      1     1 <NA>  "((comment)? @comment (call function: (identifier)) @call)\n"           5
      
      $matched_captures
      # A data frame: 9 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name    code       
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>   <chr>      
      1     2       1     1          1        3         1            1       1          4 call    f()        
      2     1       1     2          5       13         2            1       2         10 comment # comment  
      3     2       1     2         15       17         3            1       3          4 call    g()        
      4     1       1     3          5       13         2            1       2         10 comment # comment  
      5     2       1     3         41       43         5            1       5          4 call    h()        
      6     1       1     4         18       27         3            4       3         14 comment # comment1 
      7     2       1     4         41       43         5            1       5          4 call    h()        
      8     1       1     5         29       39         4            1       4         12 comment # comment 2
      9     2       1     5         41       43         5            1       5          4 call    h()        
      

# code_query, grouping sibling nodes

    Code
      code_query("f(); g(); h()", "((call) @call-1 (call) @call-2 (call) @call-3)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                            match_count
        <int> <chr> <chr>                                                    <int>
      1     1 <NA>  "((call) @call-1 (call) @call-2 (call) @call-3)\n"           1
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name   code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>  <chr>
      1     1       1     1          1        3         1            1       1          4 call-1 f()  
      2     2       1     1          6        8         1            6       1          9 call-2 g()  
      3     3       1     1         11       13         1           11       1         14 call-3 h()  
      

# code_query, alternations

    Code
      code_query("f(x=1); f(1)", paste0("(call function: (identifier) arguments: (arguments argument: [", "  (argument value: (_)) ", "  (argument name: (identifier) @arg-name value: (_))", "]))"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                           match_count
        <int> <chr> <chr>                                                                                                                                                   <int>
      1     1 <NA>  "(call function: (identifier) arguments: (arguments argument: [  (argument value: (_))   (argument name: (identifier) @arg-name value: (_))]))\n"           2
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name     code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>    <chr>
      1     1       1     1          3        3         1            3       1          4 arg-name x    
      

# code_query, wildcard node

    Code
      code_query("for (i in 1:10) foo", "(for_statement sequence: (_) @seq)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                match_count
        <int> <chr> <chr>                                        <int>
      1     1 <NA>  "(for_statement sequence: (_) @seq)\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1         11       14         1           11       1         15 seq   1:10 
      

# code_query, anchors

    Code
      code_query("f()\n# comment\ng()# comment1\n# comment 2\nh()\n", "((comment)? @comment . (call function: (identifier)) @call)")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                         match_count
        <int> <chr> <chr>                                                                 <int>
      1     1 <NA>  "((comment)? @comment . (call function: (identifier)) @call)\n"           3
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name    code       
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr>   <chr>      
      1     2       1     1          1        3         1            1       1          4 call    f()        
      2     1       1     2          5       13         2            1       2         10 comment # comment  
      3     2       1     2         15       17         3            1       3          4 call    g()        
      4     1       1     3         29       39         4            1       4         12 comment # comment 2
      5     2       1     3         41       43         5            1       5          4 call    h()        
      

---

    Code
      code_query("f(); f(x); f(x,y,z); f(x = 1, 2)", "(call arguments: (arguments . (argument) @arg))")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                             match_count
        <int> <chr> <chr>                                                     <int>
      1     1 <NA>  "(call arguments: (arguments . (argument) @arg))\n"           3
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          8        8         1            8       1          9 arg   x    
      2     1       1     2         14       14         1           14       1         15 arg   x    
      3     1       1     3         24       28         1           24       1         29 arg   x = 1
      

# code_query, predicates, #eq?

    Code
      do("", "", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                match_count
        <int> <chr> <chr>                                                                                                                                                        <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("", "", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                match_count
        <int> <chr> <chr>                                                                                                                                                        <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("*", "*", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  f    
      5     2       1     1         23       23         6            1       6          2 fn-2  f    
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      
    Code
      do("*", "*", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     2       1     1          5        5         2            1       2          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  f    
      5     2       1     1         23       23         6            1       6          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "f(1)", "f(2)", "# c", "f()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("?", "?", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "f(1)", "f(2)", "# c", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#eq? @fn-1 @fn-2)\n    )\n"           3
      
      $matched_captures
      # A data frame: 6 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1         19       19         5            1       5          2 fn-2  f    
      3     1       1     2          5        5         2            1       2          2 fn-1  f    
      4     2       1     2         19       19         5            1       5          2 fn-2  f    
      5     1       1     3         10       10         3            1       3          2 fn-1  f    
      6     2       1     3         19       19         5            1       5          2 fn-2  f    
      
    Code
      do("?", "?", c("f()", "# c", "f(5)", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#eq? @fn-1 @fn-2)\n    )\n"           2
      
      $matched_captures
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      3     1       1     2          1        1         1            1       1          2 fn-1  f    
      4     2       1     2         14       14         4            1       4          2 fn-2  f    
      
    Code
      do("?", "?", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      
    Code
      do("?", "?", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                  match_count
        <int> <chr> <chr>                                                                                                                                                          <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     2       1     1          5        5         2            1       2          2 fn-2  f    
      

# code_query, predicates, #not-eq?

    Code
      do("", "", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                    match_count
        <int> <chr> <chr>                                                                                                                                                            <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                    match_count
        <int> <chr> <chr>                                                                                                                                                            <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("*", "*", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "h()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  h    
      5     2       1     1         23       23         6            1       6          2 fn-2  g    
      
    Code
      do("*", "*", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      
    Code
      do("*", "*", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     2       1     1          5        5         2            1       2          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("+", "+", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "f(1)", "f(2)", "# c", "h()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  h    
      5     2       1     1         23       23         6            1       6          2 fn-2  g    
      
    Code
      do("+", "+", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("?", "?", c("f()", "f(1)", "f(2)", "# c", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           3
      
      $matched_captures
      # A data frame: 6 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1         19       19         5            1       5          2 fn-2  g    
      3     1       1     2          5        5         2            1       2          2 fn-1  f    
      4     2       1     2         19       19         5            1       5          2 fn-2  g    
      5     1       1     3         10       10         3            1       3          2 fn-1  f    
      6     2       1     3         19       19         5            1       5          2 fn-2  g    
      
    Code
      do("?", "?", c("f()", "# c", "g(5)", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           2
      
      $matched_captures
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      3     1       1     2          1        1         1            1       1          2 fn-1  f    
      4     2       1     2         14       14         4            1       4          2 fn-2  g    
      
    Code
      do("?", "?", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      
    Code
      do("?", "?", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     2       1     1          5        5         2            1       2          2 fn-2  f    
      

# code_query, predicates, #any-eq?

    Code
      do("", "", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                    match_count
        <int> <chr> <chr>                                                                                                                                                            <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#any-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("", "", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                    match_count
        <int> <chr> <chr>                                                                                                                                                            <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("*", "*", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  f    
      5     2       1     1         23       23         6            1       6          2 fn-2  f    
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "f(1)", "g(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  g    
      4     2       1     1         19       19         5            1       5          2 fn-2  f    
      5     2       1     1         23       23         6            1       6          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      
    Code
      do("?", "?", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "f(1)", "g(2)", "# c", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           2
      
      $matched_captures
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1         19       19         5            1       5          2 fn-2  f    
      3     1       1     2          5        5         2            1       2          2 fn-1  f    
      4     2       1     2         19       19         5            1       5          2 fn-2  f    
      
    Code
      do("?", "?", c("g()", "# c", "f(5)", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                      match_count
        <int> <chr> <chr>                                                                                                                                                              <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

# code_query, predicates, #any-not-eq?

    Code
      do("", "", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                        match_count
        <int> <chr> <chr>                                                                                                                                                                <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                        match_count
        <int> <chr> <chr>                                                                                                                                                                <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) \n      (comment)\n      (call function: (identifier) @fn-2) \n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("*", "*", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  g    
      5     2       1     1         23       23         6            1       6          2 fn-2  g    
      
    Code
      do("*", "*", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "*", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) *\n      (comment)\n      (call function: (identifier) @fn-2) *\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("+", "+", c("f()", "f(1)", "g(2)", "# c", "f()", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  g    
      4     2       1     1         19       19         5            1       5          2 fn-2  f    
      5     2       1     1         23       23         6            1       6          2 fn-2  f    
      
    Code
      do("+", "+", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 5 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     1       1     1          5        5         2            1       2          2 fn-1  f    
      3     1       1     1         10       10         3            1       3          2 fn-1  f    
      4     2       1     1         19       19         5            1       5          2 fn-2  g    
      5     2       1     1         23       23         6            1       6          2 fn-2  g    
      
    Code
      do("+", "+", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "+", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) +\n      (comment)\n      (call function: (identifier) @fn-2) +\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("f()", "# c", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  f    
      2     2       1     1          9        9         3            1       3          2 fn-2  g    
      
    Code
      do("?", "?", c("f()", "f(1)", "g(2)", "# c", "f(5)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1         10       10         3            1       3          2 fn-1  g    
      2     2       1     1         19       19         5            1       5          2 fn-2  f    
      
    Code
      do("?", "?", c("g()", "# c", "f(5)", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           2
      
      $matched_captures
      # A data frame: 4 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn-1  g    
      2     2       1     1          9        9         3            1       3          2 fn-2  f    
      3     1       1     2          1        1         1            1       1          2 fn-1  g    
      4     2       1     2         14       14         4            1       4          2 fn-2  f    
      
    Code
      do("?", "?", c("f()", "# c"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "?", c("# c", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                                                                          match_count
        <int> <chr> <chr>                                                                                                                                                                  <int>
      1     1 <NA>  "(program\n      (call function: (identifier) @fn-1) ?\n      (comment)\n      (call function: (identifier) @fn-2) ?\n      (#any-not-eq? @fn-1 @fn-2)\n    )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

# code_query, predicates, #eq? vs string

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                   match_count
        <int> <chr> <chr>                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                   match_count
        <int> <chr> <chr>                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                    match_count
        <int> <chr> <chr>                                                                                            <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      

# code_query, predicates, #not-eq? vs string

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                       match_count
        <int> <chr> <chr>                                                                                               <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                       match_count
        <int> <chr> <chr>                                                                                               <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

# code_query, predicates, #any-eq? vs string

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                       match_count
        <int> <chr> <chr>                                                                                               <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                       match_count
        <int> <chr> <chr>                                                                                               <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      3     1       1     1          9        9         3            1       3          2 fn    f    
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      

# code_query, predicates, #any-not-eq? vs string

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                           match_count
        <int> <chr> <chr>                                                                                                   <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                           match_count
        <int> <chr> <chr>                                                                                                   <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      3     1       1     1          9        9         3            1       3          2 fn    f    
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-eq? @fn \"f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-eq? @fn \"f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

# code_query, predicates, #match?

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                       match_count
        <int> <chr> <chr>                                                                                               <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                       match_count
        <int> <chr> <chr>                                                                                               <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                        match_count
        <int> <chr> <chr>                                                                                                <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      

# code_query, predicates, #not-match?

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                           match_count
        <int> <chr> <chr>                                                                                                   <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                           match_count
        <int> <chr> <chr>                                                                                                   <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

# code_query, predicates, #any-match?

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                           match_count
        <int> <chr> <chr>                                                                                                   <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                           match_count
        <int> <chr> <chr>                                                                                                   <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      3     1       1     1          9        9         3            1       3          2 fn    f    
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    f    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                            match_count
        <int> <chr> <chr>                                                                                                    <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      

# code_query, predicates, #any-not-match?

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("*", c("f()", "g()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 3 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      3     1       1     1          9        9         3            1       3          2 fn    f    
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        5         2            1       2          2 fn    g    
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-match? @fn \"^f\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                match_count
        <int> <chr> <chr>                                                                                                        <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-not-match? @fn \"^f\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

# code_query, predicates, #any-of?

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                              match_count
        <int> <chr> <chr>                                                                                                      <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                              match_count
        <int> <chr> <chr>                                                                                                      <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        6         2            1       2          3 fn    f2   
      
    Code
      do("*", c("f()", "g()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      2     1       1     1          5        6         2            1       2          3 fn    f2   
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                               match_count
        <int> <chr> <chr>                                                                                                       <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    f    
      

# code_query, predicates, #not-any-of?

    Code
      do("", "f()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                  match_count
        <int> <chr> <chr>                                                                                                          <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("", "g()")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                  match_count
        <int> <chr> <chr>                                                                                                          <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) \n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("*", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("f()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("*", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("*", c("f()", "g()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) *\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("f()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("+", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 2 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      2     1       1     1          5        5         2            1       2          2 fn    h    
      
    Code
      do("+", c("f()", "g(1)"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) +\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", "")
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("f()", "f2()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      
    Code
      do("?", c("g()", "h()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           1
      
      $matched_captures
      # A data frame: 1 x 11
           id pattern match start_byte end_byte start_row start_column end_row end_column name  code 
        <int>   <int> <int>      <int>    <int>     <int>        <int>   <int>      <int> <chr> <chr>
      1     1       1     1          1        1         1            1       1          2 fn    g    
      
    Code
      do("?", c("f()", "g()"))
    Output
      $patterns
      # A data frame: 1 x 4
           id name  pattern                                                                                                   match_count
        <int> <chr> <chr>                                                                                                           <int>
      1     1 <NA>  "(program .\n      (call function: (identifier) @fn) ?\n      (#not-any-of? @fn \"f\" \"f2\")\n    . )\n"           0
      
      $matched_captures
      # A data frame: 0 x 11
      # i 11 variables: id <int>, pattern <int>, match <int>, start_byte <int>, end_byte <int>, start_row <int>, start_column <int>, end_row <int>, end_column <int>, name <chr>, code <chr>
      

