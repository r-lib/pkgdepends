# scan_deps errors

    Code
      scan_deps(1:10)
    Condition
      Error:
      ! `path` must be a character vector without `NA`, but it is an integer vector
    Code
      scan_deps(root = mtcars)
    Condition
      Error:
      ! `root` must be a string (character scalar), but it is a data frame.
    Code
      scan_deps(root = "sdfssdfsdf")
    Condition
      Error:
      ! Project root 'sdfssdfsdf' does not exist.
    Code
      scan_deps("sdfssdfsdf")
    Condition
      Error:
      ! Path does not exist: 'sdfssdfsdf'.
    Code
      scan_deps(c("sdfssdfsdf-1", "sdfssdfsdf-2"))
    Condition
      Error:
      ! Paths do not exist: 'sdfssdfsdf-1' and 'sdfssdfsdf-2'.
    Code
      scan_deps("foo", "bar")
    Condition
      Error:
      ! 'foo' is outside of project root 'bar'.
    Code
      scan_deps(c("foo", "foobar"), "bar")
    Condition
      Error:
      ! 'foo' and 'foobar' are outside of project root 'bar'.

# get_deps_cache_path

    Code
      writeLines(get_deps_cache_path())
    Output
      <tempdir>/<tempfile>/R/pkgcache/deps/2
    Code
      writeLines(get_deps_cache_path("badcafe"))
    Output
      <tempdir>/<tempfile>/R/pkgcache/deps/2/ba/badcafe

# clear_deps_cache

    Code
      dir(tmp, recursive = TRUE)
    Output
      [1] "R/pkgcache/deps/2/ba/badcafe"

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

# scan_deps_df

    Code
      scan_deps_df()
    Output
      # A data frame: 0 x 9
      # i 9 variables: path <chr>, ref <chr>, package <chr>, version <chr>,
      #   type <chr>, code <chr>, start_row <int>, start_column <int>,
      #   start_byte <int>

# scan_path_deps_do

    Code
      scan_path_deps_do(readLines(rfile), basename(rfile))
    Output
      # A data frame: 3 x 9
        path   ref   package version type  code      start_row start_column start_byte
        <chr>  <chr> <chr>   <chr>   <chr> <chr>         <int>        <int>      <int>
      1 code.R CD    CD      *       prod  CD::pkg           4            1         26
      2 code.R AB    AB      *       prod  library(~         1            1          1
      3 code.R BC    BC      *       prod  require(~         2            1         13
    Code
      scan_path_deps_do(readLines(nsfile), basename(nsfile))
    Output
      # A data frame: 2 x 9
        path      ref   package version type  code  start_row start_column start_byte
        <chr>     <chr> <chr>   <chr>   <chr> <chr>     <int>        <int>      <int>
      1 NAMESPACE stats stats   *       prod  stats         1            1          1
      2 NAMESPACE utils utils   *       prod  utils         1            1          1

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
      # A data frame: 3 x 9
        path                             ref   package version type  code        start_row start_column start_byte
        <chr>                            <chr> <chr>   <chr>   <chr> <chr>           <int>        <int>      <int>
      1 fixtures/scan/project-1/R/code.R CD    CD      *       prod  CD::pkg             4            1         26
      2 fixtures/scan/project-1/R/code.R AB    AB      *       prod  library(AB)         1            1          1
      3 fixtures/scan/project-1/R/code.R BC    BC      *       prod  require(BC)         2            1         13

# scan_path_deps_do_fn_hits

    Code
      scan_path_deps_do_r(readLines(rfile), rfile)
    Output
      # A data frame: 2 x 9
        path                    ref     package version type  code                                                               start_row start_column start_byte
        <chr>                   <chr>   <chr>   <chr>   <chr> <chr>                                                                  <int>        <int>      <int>
      1 fixtures/scan/methods.R methods methods *       prod  "setClass(\"track\", slots = c(x = \"numeric\", y = \"numeric\"))"         2           10         43
      2 fixtures/scan/methods.R methods methods *       prod  "setGeneric(\"plot\")"                                                     6            1        175

# scan_path_deps_do_jr_hits

    Code
      scan_path_deps_do_r(readLines(rfile), rfile)
    Output
      # A data frame: 6 x 9
        path                  ref      package  version type  code                          start_row start_column start_byte
        <chr>                 <chr>    <chr>    <chr>   <chr> <chr>                             <int>        <int>      <int>
      1 fixtures/scan/junit.R testthat testthat *       prod  testthat::JunitReporter               1            8          8
      2 fixtures/scan/junit.R testthat testthat *       prod  library(testthat)                     3            1         39
      3 fixtures/scan/junit.R xml2     xml2     *       prod  testthat::JunitReporter$new()         1            8          8
      4 fixtures/scan/junit.R xml2     xml2     *       prod  JunitReporter$new()                   5            9         66
      5 fixtures/scan/junit.R xml2     xml2     *       prod  JunitReporter                         1           18         18
      6 fixtures/scan/junit.R xml2     xml2     *       prod  JunitReporter                         5            9         66

# scan_pat_deps_do_ragg_hits

    Code
      scan_path_deps_do_rmd(readLines(rfile), rfile)
    Output
      # A data frame: 4 x 9
        path                    ref       package   version type  code                                        start_row start_column start_byte
        <chr>                   <chr>     <chr>     <chr>   <chr> <chr>                                           <int>        <int>      <int>
      1 fixtures/scan/knitr.Rmd knitr     knitr     *       prod  "knitr::opts_chunk"                                 3            1          9
      2 fixtures/scan/knitr.Rmd knitr     knitr     *       prod  "knitr::opts_chunk"                                 7            1         61
      3 fixtures/scan/knitr.Rmd ragg      ragg      *       prod  "knitr::opts_chunk$set(dev = \"ragg_png\")"         3            1          9
      4 fixtures/scan/knitr.Rmd rmarkdown rmarkdown *       prod  "r"                                                 2            5          6

---

    Code
      scan_path_deps_do_rmd(readLines(rfile), rfile)
    Output
      # A data frame: 2 x 9
        path                     ref       package   version type  code              start_row start_column start_byte
        <chr>                    <chr>     <chr>     <chr>   <chr> <chr>                 <int>        <int>      <int>
      1 fixtures/scan/noragg.Rmd knitr     knitr     *       prod  knitr::opts_chunk         2            1          8
      2 fixtures/scan/noragg.Rmd rmarkdown rmarkdown *       prod  r                         1            5          5

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
      # A data frame: 2 x 9
        path             ref       package   version type  code           start_row start_column start_byte
        <chr>            <chr>     <chr>     <chr>   <chr> <chr>              <int>        <int>      <int>
      1 chunk-errors.Rmd dplyr     dplyr     *       prod  library(dplyr)         8            1        115
      2 chunk-errors.Rmd rmarkdown rmarkdown *       prod  r                      7            5        112

# scan_path_deps_do_rmd #2

    Code
      scan_path_deps_do_rmd(readLines(path), "inline-chunks.Rmd")
    Output
      # A data frame: 3 x 9
        path              ref      package  version type  code             start_row start_column start_byte
        <chr>             <chr>    <chr>    <chr>   <chr> <chr>                <int>        <int>      <int>
      1 inline-chunks.Rmd inline   inline   *       prod  inline::chunks           4           49         68
      2 inline-chunks.Rmd multiple multiple *       prod  multiple::calls          4           92        111
      3 inline-chunks.Rmd separate separate *       prod  separate::chunks         6           12        160

# scan_path_deps_do_rmd #3

    Code
      scan_path_deps_do_rmd(readLines(path), "nothing.Rmd")
    Output
      # A data frame: 1 x 9
        path        ref       package   version type  code  start_row start_column start_byte
        <chr>       <chr>     <chr>     <chr>   <chr> <chr>     <int>        <int>      <int>
      1 nothing.Rmd rmarkdown rmarkdown *       prod  r             7            5         90

# scan_path_deps_do_header_hits

    Code
      scan_path_deps_do_rmd(readLines(path), basename(path))
    Output
      # A data frame: 2 x 9
        path       ref   package version type  code        start_row start_column start_byte
        <chr>      <chr> <chr>   <chr>   <chr> <chr>           <int>        <int>      <int>
      1 header.Rmd p1    p1      *       prod  p1::fun             4           14         32
      2 header.Rmd p2    p2      *       prod  library(p2)         7           14         81

# scan_path_deps_do_header_shiny_hits

    Code
      scan_path_deps_do_rmd(readLines(path), basename(path))
    Output
      # A data frame: 4 x 9
        path             ref   package version type  code         start_row start_column start_byte
        <chr>            <chr> <chr>   <chr>   <chr> <chr>            <int>        <int>      <int>
      1 header-shiny.Rmd shiny shiny   *       prod  "shiny"              4           11         26
      2 header-shiny.Rmd shiny shiny   *       prod  "'shiny'"            5            9         40
      3 header-shiny.Rmd shiny shiny   *       prod  "\"shiny\""          6           11         58
      4 header-shiny.Rmd shiny shiny   *       prod  "|\n  shiny"         7            9         74

---

    Code
      scan_path_deps_do_rmd(readLines(path), basename(path))
    Output
      # A data frame: 4 x 9
        path              ref   package version type  code           start_row start_column start_byte
        <chr>             <chr> <chr>   <chr>   <chr> <chr>              <int>        <int>      <int>
      1 header-shiny2.Rmd shiny shiny   *       prod  "shiny"                5            9         32
      2 header-shiny2.Rmd shiny shiny   *       prod  "'shiny'"              7            9         56
      3 header-shiny2.Rmd shiny shiny   *       prod  "\"shiny\""            9           11         82
      4 header-shiny2.Rmd shiny shiny   *       prod  ">\n    shiny"        11            9        106

# scan_path_deps_do_header_bslib_hits

    Code
      scan_path_deps_do_rmd(readLines(path), basename(path))
    Output
      # A data frame: 1 x 9
        path             ref   package version type  code                                                              start_row start_column start_byte
        <chr>            <chr> <chr>   <chr>   <chr> <chr>                                                                 <int>        <int>      <int>
      1 header-bslib.Rmd bslib bslib   *       prod  "output:\n  html_document:\n    toc: true\n    theme: some theme"         4            1         16

# scan_path_deps_do_dsc

    Code
      print(scan_path_deps_do_dsc(readLines(path), basename(path)), n = Inf)
    Output
      # A data frame: 42 x 9
         path        ref                    package      version      type  code                   start_row start_column start_byte
         <chr>       <chr>                  <chr>        <chr>        <chr> <chr>                      <int>        <int>      <int>
       1 DESCRIPTION callr                  callr        >=3.3.1      prod  callr                          1            1          1
       2 DESCRIPTION r-lib/cli              cli          >=3.6.0      prod  r-lib/cli                      1            1          1
       3 DESCRIPTION curl                   curl         *            prod  curl                           1            1          1
       4 DESCRIPTION desc                   desc         >=1.4.3      prod  desc                           1            1          1
       5 DESCRIPTION filelock               filelock     >=1.0.2      prod  filelock                       1            1          1
       6 DESCRIPTION jsonlite               jsonlite     *            prod  jsonlite                       1            1          1
       7 DESCRIPTION lpSolve                lpSolve      *            prod  lpSolve                        1            1          1
       8 DESCRIPTION pkgbuild               pkgbuild     >=1.0.2      prod  pkgbuild                       1            1          1
       9 DESCRIPTION pkgcache               pkgcache     >=2.2.0      prod  pkgcache                       1            1          1
      10 DESCRIPTION processx               processx     >=3.4.2      prod  processx                       1            1          1
      11 DESCRIPTION ps                     ps           *            prod  ps                             1            1          1
      12 DESCRIPTION R6                     R6           *            prod  R6                             1            1          1
      13 DESCRIPTION zip                    zip          >=2.3.0      prod  zip                            1            1          1
      14 DESCRIPTION asciicast              asciicast    >=2.2.0.9000 test  asciicast                      1            1          1
      15 DESCRIPTION codetools              codetools    *            test  codetools                      1            1          1
      16 DESCRIPTION covr                   covr         *            test  covr                           1            1          1
      17 DESCRIPTION debugme                debugme      *            test  debugme                        1            1          1
      18 DESCRIPTION fansi                  fansi        *            test  fansi                          1            1          1
      19 DESCRIPTION fs                     fs           *            test  fs                             1            1          1
      20 DESCRIPTION gh                     gh           *            test  gh                             1            1          1
      21 DESCRIPTION gitcreds               gitcreds     *            test  gitcreds                       1            1          1
      22 DESCRIPTION glue                   glue         *            test  glue                           1            1          1
      23 DESCRIPTION htmlwidgets            htmlwidgets  *            test  htmlwidgets                    1            1          1
      24 DESCRIPTION mockery                mockery      *            test  mockery                        1            1          1
      25 DESCRIPTION pak                    pak          *            test  pak                            1            1          1
      26 DESCRIPTION pingr                  pingr        >=2.0.0      test  pingr                          1            1          1
      27 DESCRIPTION rmarkdown              rmarkdown    *            test  rmarkdown                      1            1          1
      28 DESCRIPTION rstudioapi             rstudioapi   *            test  rstudioapi                     1            1          1
      29 DESCRIPTION spelling               spelling     *            test  spelling                       1            1          1
      30 DESCRIPTION svglite                svglite      *            test  svglite                        1            1          1
      31 DESCRIPTION testthat               testthat     >=3.2.0      test  testthat                       1            1          1
      32 DESCRIPTION tibble                 tibble       *            test  tibble                         1            1          1
      33 DESCRIPTION webfakes               webfakes     >=1.1.5.9000 test  webfakes                       1            1          1
      34 DESCRIPTION withr                  withr        >=2.1.1      test  withr                          1            1          1
      35 DESCRIPTION gh                     gh           *            dev   gh                             1            1          1
      36 DESCRIPTION pkgsearch              pkgsearch    *            dev   pkgsearch                      1            1          1
      37 DESCRIPTION withr                  withr        >=2.1.1      dev   withr                          1            1          1
      38 DESCRIPTION r-lib/asciicast        asciicast    *            test  r-lib/asciicast                1            1          1
      39 DESCRIPTION covr                   covr         *            test  covr                           1            1          1
      40 DESCRIPTION r-lib/asciicast        asciicast    *            dev   r-lib/asciicast                1            1          1
      41 DESCRIPTION pkgdown                pkgdown      >=2.0.2      dev   pkgdown                        1            1          1
      42 DESCRIPTION tidyverse/tidytemplate tidytemplate *            dev   tidyverse/tidytemplate         1            1          1

# scan_path_deps_do_namespace

    Code
      print(scan_path_deps_do_namespace(readBin(path, "raw", 10000), path), n = Inf)
    Output
      # A data frame: 2 x 9
        path                    ref   package version type  code  start_row start_column start_byte
        <chr>                   <chr> <chr>   <chr>   <chr> <chr>     <int>        <int>      <int>
      1 fixtures/scan/NAMESPACE stats stats   *       prod  stats         1            1          1
      2 fixtures/scan/NAMESPACE utils utils   *       prod  utils         1            1          1

# scan_path_deps_do_{bookdown,pkgdown,quarto,renv_lock,rsconnect}

    Code
      scan_deps()[]
    Output
      # A data frame: 4 x 9
        path                   ref       package   version type  code  start_row start_column start_byte
        <chr>                  <chr>     <chr>     <chr>   <chr> <chr>     <int>        <int>      <int>
      1 bookdown/_bookdown.yml bookdown  bookdown  *       prod  <NA>          1            1          1
      2 _pkgdown.yml           pkgdown   pkgdown   *       prod  <NA>          1            1          1
      3 renv.lock              renv      renv      *       prod  <NA>          1            1          1
      4 rsconnect              rsconnect rsconnect *       prod  <NA>          1            1          1

# .Rproj file

    Code
      scan_deps(project, root = project)[]
    Output
      # A data frame: 2 x 9
        path             ref      package  version type  code                    start_row start_column start_byte
        <chr>            <chr>    <chr>    <chr>   <chr> <chr>                       <int>        <int>      <int>
      1 pkgdepends.Rproj devtools devtools *       prod  PackageUseDevtools: Yes         1            1          1
      2 pkgdepends.Rproj roxygen2 roxygen2 *       prod  PackageUseDevtools: Yes         1            1          1

# scan_path_deps_do_rnw_ranges

    Code
      scan_path_deps_do_rnw_ranges(code)
    Output
      [[1]]
      [1] 11 13
      
      [[2]]
      [1] 15 17
      
      [[3]]
      [1] 19 21
      
      [[4]]
      [1] 22 25
      
      [[5]]
      [1] 27 29
      

# scan_path_deps_do_rnw_parse_chunk_header

    Code
      scan_path_deps_do_rnw_parse_chunk_header("")
    Output
      named list()
    Code
      scan_path_deps_do_rnw_parse_chunk_header("name")
    Output
      $label
      [1] "name"
      
    Code
      scan_path_deps_do_rnw_parse_chunk_header(
        "name, foo = 1, bar = TRUE, this = that")
    Output
      $label
      [1] "name"
      
      $foo
      [1] 1
      
      $bar
      [1] TRUE
      
      $this
      that
      

# .Rnw file

    Code
      scan_path_deps_do_rnw(readLines(path), basename(path))
    Output
      # A data frame: 3 x 9
        path     ref      package  version type  code           start_row start_column start_byte
        <chr>    <chr>    <chr>    <chr>   <chr> <chr>              <int>        <int>      <int>
      1 test.Rnw cli      cli      *       prod  library(cli)           1            1          1
      2 test.Rnw filelock filelock *       prod  filelock::lock         1            1          1
      3 test.Rnw pak      pak      *       prod  library(pak)           2            1         43
    Code
      scan_path_deps_do(readLines(path), basename(path))
    Output
      # A data frame: 3 x 9
        path     ref      package  version type  code           start_row start_column start_byte
        <chr>    <chr>    <chr>    <chr>   <chr> <chr>              <int>        <int>      <int>
      1 test.Rnw cli      cli      *       prod  library(cli)           1            1          1
      2 test.Rnw filelock filelock *       prod  filelock::lock         1            1          1
      3 test.Rnw pak      pak      *       prod  library(pak)           2            1         43

# Ignored chunks in .Rnw file

    Code
      scan_path_deps_do(readBin(path, "raw", file.size(path)), basename(path))
    Output
      # A data frame: 6 x 9
        path            ref           package       version type  code                   start_row start_column start_byte
        <chr>           <chr>         <chr>         <chr>   <chr> <chr>                      <int>        <int>      <int>
      1 ignore-test.Rnw good          good          *       prod  library(good)                  1            1          1
      2 ignore-test.Rnw butthisisgood butthisisgood *       prod  library(butthisisgood)         1            1          1
      3 ignore-test.Rnw butthisisgood butthisisgood *       prod  library(butthisisgood)         1            1          1
      4 ignore-test.Rnw good          good          *       prod  library(good)                  1            1          1
      5 ignore-test.Rnw good          good          *       prod  library(good)                  1            1          1
      6 ignore-test.Rnw good          good          *       prod  library(good)                  1            1          1

# IPython notebook

    Code
      scan_path_deps_do(readLines(path), basename(path))
    Output
      # A data frame: 4 x 9
        path           ref      package  version type  code            start_row start_column start_byte
        <chr>          <chr>    <chr>    <chr>   <chr> <chr>               <int>        <int>      <int>
      1 notebook.ipynb IRkernel IRkernel *       prod  <NA>                    1            1          1
      2 notebook.ipynb MASS     MASS     *       prod  library(MASS)           1            1          1
      3 notebook.ipynb stats    stats    *       prod  stats::setNames         1            1          1
      4 notebook.ipynb cli      cli      *       prod  cli::cli_text           2            1         19

