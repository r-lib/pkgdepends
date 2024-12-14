# scan_deps

    Code
      scan_deps(project)[]
    Output
      # A data frame: 6 x 9
        path      ref     package version type  code              start_row start_column start_byte
        <chr>     <chr>   <chr>   <chr>   <chr> <chr>                 <int>        <int>      <int>
      1 R/code.R  CD      CD      *       prod  CD::pkg                   4            1         26
      2 R/code.R  AB      AB      *       prod  library(AB)               1            1          1
      3 R/code.R  BC      BC      *       prod  require(BC)               2            1         13
      4 doc.qmd   pkgload pkgload *       prod  pkgload::load_all        12            1        174
      5 index.Rmd ST      ST      *       prod  ST::fun                  10            1         97
      6 index.Rmd RS      RS      *       prod  library(RS)               9            1         85

---

    Code
      scan_deps(project)
    Output
      
      Dependencies:
      + AB      @ R/code.R
      + BC      @ R/code.R
      + CD      @ R/code.R
      + RS      @ index.Rmd
      + ST      @ index.Rmd
      + pkgload @ doc.qmd

# scan_path_deps

    Code
      scan_path_deps(rfile)
    Output
      # A data frame: 3 x 9
        path                             ref   package version type  code        start_row start_column start_byte
        <chr>                            <chr> <chr>   <chr>   <chr> <chr>           <int>        <int>      <int>
      1 fixtures/scan/project-1/R/code.R CD    CD      *       prod  CD::pkg             4            1         26
      2 fixtures/scan/project-1/R/code.R AB    AB      *       prod  library(AB)         1            1          1
      3 fixtures/scan/project-1/R/code.R BC    BC      *       prod  require(BC)         2            1         13

---

    Code
      scan_path_deps(rfile)
    Output
      # A data frame: 3 x 9
        path                             ref   package version type  code        start_row start_column start_byte
        <chr>                            <chr> <chr>   <chr>   <chr> <chr>           <int>        <int>      <int>
      1 fixtures/scan/project-1/R/code.R CD    CD      *       prod  CD::pkg             4            1         26
      2 fixtures/scan/project-1/R/code.R AB    AB      *       prod  library(AB)         1            1          1
      3 fixtures/scan/project-1/R/code.R BC    BC      *       prod  require(BC)         2            1         13

