# get_deps_cache_path

    Code
      get_deps_cache_path()
    Output
      [1] "<tempdir>/<tempfile>/R/pkgcache/deps/1"
    Code
      get_deps_cache_path("badcafe")
    Output
      [1] "<tempdir>/<tempfile>/R/pkgcache/deps/1/ba/badcafe"

# clear_deps_cache

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "R/pkgcache/deps/1/ba/badcafe"

---

    Code
      dir(tmp, recursive = TRUE)
    Output
      character(0)

# re_r_dep

    Code
      re_r_dep()
    Output
      [1] "library|require|loadNamespace|::|setClass|setGeneric|pkg_attach|p_load|module|import|box::|tar_option_set|glue|ggsave|set_engine|opts_chunk|geom_hex|JunitReporter|geom_hex|JunitReporter"

# scan_path_deps_empty

    Code
      scan_path_deps_empty()
    Output
      # A data frame: 0 x 7
      # i 7 variables: path <chr>, package <chr>, type <chr>, code <chr>,
      #   start_row <int>, start_column <int>, start_byte <int>

# scan_path_deps_do

    Code
      scan_path_deps_do(readLines(rfile), basename(rfile))
    Output
      # A data frame: 3 x 7
        path   package type  code        start_row start_column start_byte
        <chr>  <chr>   <chr> <chr>           <int>        <int>      <int>
      1 code.R CD      prod  CD::pkg             4            1         26
      2 code.R AB      prod  library(AB)         1            1          1
      3 code.R BC      prod  require(BC)         2            1         13

---

    Code
      scan_path_deps_do("code", "foo.unknown")
    Condition
      Error:
      ! Cannot parse .unknown file for dependencies, internal error

# scan_path_deps_do_r

    Code
      scan_path_deps_do_r(readLines(rfile), rfile)
    Output
      # A data frame: 3 x 7
        path                             package type  code        start_row start_column start_byte
        <chr>                            <chr>   <chr> <chr>           <int>        <int>      <int>
      1 fixtures/scan/project-1/R/code.R CD      prod  CD::pkg             4            1         26
      2 fixtures/scan/project-1/R/code.R AB      prod  library(AB)         1            1          1
      3 fixtures/scan/project-1/R/code.R BC      prod  require(BC)         2            1         13

# scan_path_deps_do_fn_hits

    Code
      scan_path_deps_do_r(readLines(rfile), rfile)
    Output
      # A data frame: 2 x 7
        path                    package type  code                                                           start_row start_column start_byte
        <chr>                   <chr>   <chr> <chr>                                                              <int>        <int>      <int>
      1 fixtures/scan/methods.R methods prod  "setClass(\"track\", slots = c(x=\"numeric\", y=\"numeric\"))"         2           10         43
      2 fixtures/scan/methods.R methods prod  "setGeneric(\"plot\")"                                                 6            1        171

# scan_path_deps_do_jr_hits

    Code
      scan_path_deps_do_r(readLines(rfile), rfile)
    Output
      # A data frame: 6 x 7
        path                  package  type  code                          start_row start_column start_byte
        <chr>                 <chr>    <chr> <chr>                             <int>        <int>      <int>
      1 fixtures/scan/junit.R testthat prod  testthat::JunitReporter               1            8          8
      2 fixtures/scan/junit.R testthat prod  library(testthat)                     3            1         39
      3 fixtures/scan/junit.R xml2     prod  testthat::JunitReporter$new()         1            8          8
      4 fixtures/scan/junit.R xml2     prod  JunitReporter$new()                   5            9         66
      5 fixtures/scan/junit.R xml2     prod  JunitReporter                         1           18         18
      6 fixtures/scan/junit.R xml2     prod  JunitReporter                         5            9         66

# scan_pat_deps_do_ragg_hits

    Code
      scan_path_deps_do_rmd(readLines(rfile), rfile)
    Output
      # A data frame: 3 x 7
        path                    package type  code                                        start_row start_column start_byte
        <chr>                   <chr>   <chr> <chr>                                           <int>        <int>      <int>
      1 fixtures/scan/knitr.Rmd knitr   prod  "knitr::opts_chunk"                                 3            1          9
      2 fixtures/scan/knitr.Rmd knitr   prod  "knitr::opts_chunk"                                 7            1         61
      3 fixtures/scan/knitr.Rmd ragg    prod  "knitr::opts_chunk$set(dev = \"ragg_png\")"         3            1          9

---

    Code
      scan_path_deps_do_rmd(readLines(rfile), rfile)
    Output
      # A data frame: 1 x 7
        path                     package type  code              start_row start_column start_byte
        <chr>                    <chr>   <chr> <chr>                 <int>        <int>      <int>
      1 fixtures/scan/noragg.Rmd knitr   prod  knitr::opts_chunk         2            1          8

# safe_parse_pkg_from_call

    Code
      safe_parse_pkg_from_call(NA_character_, "library", "library(qwe)")
    Output
      [1] "qwe"

# parse_pkg_from_call

    Code
      parse_pkg_from_call(NA_character_, "library", "library(qwe)")
    Output
      [1] "qwe"
    Code
      parse_pkg_from_call("base", "loadNamespace", "loadNamespace('q1')")
    Output
      [1] "q1"
    Code
      parse_pkg_from_call("base", "requireNamespace", "requireNamespace('q1')")
    Output
      [1] "q1"
    Code
      parse_pkg_from_call(NA_character_, "pkg_attach", "pkg_attach('foobar')")
    Output
      [1] "foobar"
    Code
      parse_pkg_from_call(NA_character_, "pkg_attach2", "pkg_attach2('foobar')")
    Output
      [1] "foobar"
    Code
      parse_pkg_from_call("pacman", "p_load", "p_load('p1')")
    Output
      [1] "p1"
    Code
      parse_pkg_from_call(NA_character_, "import", "import(x1)")
    Output
      [1] "x1"
    Code
      parse_pkg_from_call(NA_character_, "module", "module({import('x2')})")
    Output
      [1] "x2"
    Code
      parse_pkg_from_call("import", "from", "import::from(dplyr)")
    Output
      [1] "dplyr"
    Code
      parse_pkg_from_call("import", "into",
        "import::into('operators', .from = 'dplyr')")
    Output
      [1] "dplyr"
    Code
      parse_pkg_from_call("import", "here", "import::here('dplyr')")
    Output
      [1] "dplyr"
    Code
      parse_pkg_from_call("box", "use", "box::use(dplyr[filter, select])")
    Output
      [1] "dplyr"
    Code
      parse_pkg_from_call("targets", "tar_option_set",
        "tar_option_set(packages = c('p1', 'p2'))")
    Output
      [1] "p1" "p2"
    Code
      parse_pkg_from_call("glue", "glue", "glue::glue('blah {library(x5)} blah')")
    Output
      [1] "x5"
    Code
      parse_pkg_from_call(NA_character_, "ggsave", "ggsave(filename = 'foo.svg')")
    Output
      [1] "svglite"
    Code
      parse_pkg_from_call(NA_character_, "set_engine", "set_engine(engine = 'spark')")
    Output
      [1] "sparklyr"
    Code
      parse_pkg_from_call("R6", "R6Class",
        "R6::R6Class('foobar', inherit = JunitReporter)")
    Output
      [1] "xml2"
    Code
      parse_pkg_from_call("testthat", "test_dir",
        "testthat::test_dir(reporter = 'junit')")
    Output
      [1] "xml2"

# parse_pkg_from_call_library

    Code
      ppcl("library", "library(qqq)")
    Output
      [1] "qqq"
    Code
      ppcl("library", "library('qqq')")
    Output
      [1] "qqq"
    Code
      ppcl("library", "library(qqq)", ns = "base")
    Output
      [1] "qqq"
    Code
      ppcl("require", "require(qqq)")
    Output
      [1] "qqq"
    Code
      ppcl("require", "require('qqq')")
    Output
      [1] "qqq"
    Code
      ppcl("require", "require('qqq')", ns = "base")
    Output
      [1] "qqq"

# dependencies_eval

    Code
      dependencies_eval(quote({
        1:10
        c(10:1)[1:3]
      }))
    Output
      [1] 10  9  8

# scan_path_deps_do_rmd

    Code
      scan_path_deps_do_rmd(readLines(path), "chunk-errors.Rmd")
    Output
      # A data frame: 1 x 7
        path             package type  code           start_row start_column start_byte
        <chr>            <chr>   <chr> <chr>              <int>        <int>      <int>
      1 chunk-errors.Rmd dplyr   prod  library(dplyr)         8            1        115

# scan_path_deps_do_rmd #2

    Code
      scan_path_deps_do_rmd(readLines(path), "inline-chunks.Rmd")
    Output
      # A data frame: 3 x 7
        path              package  type  code             start_row start_column start_byte
        <chr>             <chr>    <chr> <chr>                <int>        <int>      <int>
      1 inline-chunks.Rmd inline   prod  inline::chunks           4           49         68
      2 inline-chunks.Rmd multiple prod  multiple::calls          4           92        111
      3 inline-chunks.Rmd separate prod  separate::chunks         6           12        160

# scan_path_deps_do_rmd #3

    Code
      scan_path_deps_do_rmd(readLines(path), "nothing.Rmd")
    Output
      NULL

