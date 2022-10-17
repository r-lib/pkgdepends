# conflicting dependencies

    Code
      dsc
    Output
      * aa: Can't install dependency cran::cc
      * cran::cc: cran::cc conflicts with cc/cc, to be installed

# conflicting dependencies downstream

    Code
      dsc
    Output
      * a0: Can't install dependency bb
      * bb: Can't install dependency cc/cc
      * cc/cc: cc/cc conflicts with cran::cc, to be installed

