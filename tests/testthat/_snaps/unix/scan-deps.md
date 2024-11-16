# scan_deps

    Code
      scan_deps(project)
    Output
      # A data frame: 6 x 7
        path      package type  code              start_row start_column start_byte
        <chr>     <chr>   <chr> <chr>                 <int>        <int>      <int>
      1 R/code.R  CD      prod  CD::pkg                   4            1         26
      2 R/code.R  AB      prod  library(AB)               1            1          1
      3 R/code.R  BC      prod  require(BC)               2            1         13
      4 doc.qmd   pkgload prod  pkgload::load_all        12            1        174
      5 index.Rmd ST      prod  ST::fun                  10            1         97
      6 index.Rmd RS      prod  library(RS)               9            1         85

# scan_path_deps

    Code
      scan_path_deps(rfile)
    Output
      # A data frame: 3 x 7
        path                             package type  code        start_row start_column start_byte
        <chr>                            <chr>   <chr> <chr>           <int>        <int>      <int>
      1 fixtures/scan/project-1/R/code.R CD      prod  CD::pkg             4            1         26
      2 fixtures/scan/project-1/R/code.R AB      prod  library(AB)         1            1          1
      3 fixtures/scan/project-1/R/code.R BC      prod  require(BC)         2            1         13

---

    Code
      scan_path_deps(rfile)
    Output
      # A data frame: 3 x 7
        path                             package type  code        start_row start_column start_byte
        <chr>                            <chr>   <chr> <chr>           <int>        <int>      <int>
      1 fixtures/scan/project-1/R/code.R CD      prod  CD::pkg             4            1         26
      2 fixtures/scan/project-1/R/code.R AB      prod  library(AB)         1            1          1
      3 fixtures/scan/project-1/R/code.R BC      prod  require(BC)         2            1         13

