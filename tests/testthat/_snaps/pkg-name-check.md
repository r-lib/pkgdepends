# pnc_base

    Code
      pnc_base("base")
    Output
      $base
      [1] FALSE
      
      $package
      [1] "base"
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

---

    Code
      pnc_base("Base")
    Output
      $base
      [1] FALSE
      
      $package
      [1] "base"
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

---

    Code
      pnc_base("TOOLS")
    Output
      $base
      [1] FALSE
      
      $package
      [1] "tools"
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

---

    Code
      pnc_base("definitely-not")
    Output
      $base
      [1] TRUE
      
      $package
      NULL
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

# wikipedia request

    Code
      show_request(ret)
    Output
      POST application/x-www-form-urlencoded
      Query string: 
      Body: action=query&format=json&prop=extracts&titles=foobar&redirects=1&exintro=1&explaintext=1

