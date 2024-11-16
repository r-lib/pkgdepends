# get_dep_type_from_path

    Code
      get_dep_type_from_path(c("R/foo.R", "man/roxygen/meta.R", "tests/test-1.R",
        "test/test-2.R"))
    Output
      [1] "prod" "dev"  "test" "test"

