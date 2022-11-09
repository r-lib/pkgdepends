# is_valid_package_name

    Code
      is_valid_package_name("foo")
    Output
      [1] TRUE
    Code
      is_valid_package_name("pkg")
    Output
      [1] FALSE
      attr(,"reason")
      [1] "Package name forbidden by CRAN."
    Code
      is_valid_package_name("fáf")
    Output
      [1] FALSE
      attr(,"reason")
      [1] "It can only contain ASCII characters."
    Code
      is_valid_package_name("foo-bar")
    Output
      [1] FALSE
      attr(,"reason")
      [1] "It can only contain letters, numbers and dot."
    Code
      is_valid_package_name("x")
    Output
      [1] FALSE
      attr(,"reason")
      [1] "It must have at least two characters."
    Code
      is_valid_package_name("1xyz")
    Output
      [1] FALSE
      attr(,"reason")
      [1] "It must start with a letter."
    Code
      is_valid_package_name("dotted.")
    Output
      [1] FALSE
      attr(,"reason")
      [1] "It must not end with a dot."

