# format.pkg_name_basics

    +------------------------------------------------------------------------------+
    |                                --*-- pwr --*--                               |
    +------------------------------------------------------------------------------+
    +------------------------------------------------------------------------------+
    | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
    +------------------------------------------------------------------------------+
    +------------------------------------------------------------------------------+
    |                               --*-- shit --*--                               |
    +------------------------------------------------------------------------------+
    +------------------------------------------------------------------------------+
    | v  valid name      v  CRAN            v  Bioconductor    x  profanity        |
    +------------------------------------------------------------------------------+

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

# format.pkg_name_check_wikipedia

    + Wikipedia -------------------------------------------------------------------+
    | CLI (from Cli) CLI may refer to multiple articles, see link.                 |
    +------------------------------------------- https://en.wikipedia.org/wiki/CLI +
    + Wikipedia -------------------------------------------------------------------+
    | Surely-not-this No definition found                                          |
    +------------------------------------------------------------------------------+

# wikipedia request

    Code
      show_request(ret)
    Output
      POST application/x-www-form-urlencoded
      Query string: 
      Body: action=query&format=json&prop=extracts&titles=foobar&redirects=1&exintro=1&explaintext=1

