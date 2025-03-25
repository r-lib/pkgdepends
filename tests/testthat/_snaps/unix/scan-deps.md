# scan_deps

    Code
      scan_deps(project, root = project)[]
    Output
      # A data frame: 11 x 9
         path            ref       package   version type  code              start_row start_column start_byte
         <chr>           <chr>     <chr>     <chr>   <chr> <chr>                 <int>        <int>      <int>
       1 R/code.R        CD        CD        *       prod  CD::pkg                   4            1         26
       2 R/code.R        AB        AB        *       prod  library(AB)               1            1          1
       3 R/code.R        BC        BC        *       prod  require(BC)               2            1         13
       4 doc.qmd         pkgload   pkgload   *       prod  pkgload::load_all        12            1        174
       5 doc.qmd         rmarkdown rmarkdown *       prod  r                        11            5        140
       6 index.Rmarkdown ST        ST        *       prod  ST::fun                  10            1         97
       7 index.Rmarkdown RS        RS        *       prod  library(RS)               9            1         85
       8 index.Rmarkdown rmarkdown rmarkdown *       prod  r                         8            5         82
       9 index.Rmd       ST        ST        *       prod  ST::fun                  10            1         97
      10 index.Rmd       RS        RS        *       prod  library(RS)               9            1         85
      11 index.Rmd       rmarkdown rmarkdown *       prod  r                         8            5         82

---

    Code
      scan_deps(project, root = project)
    Output
      
      Dependencies:
      + AB        @ R/code.R
      + BC        @ R/code.R
      + CD        @ R/code.R
      + RS        @ index.Rmarkdown, index.Rmd
      + ST        @ index.Rmarkdown, index.Rmd
      + pkgload   @ doc.qmd
      + rmarkdown @ doc.qmd, index.Rmarkdown, index.Rmd

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

