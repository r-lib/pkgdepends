# install_package_plan metadata

    Code
      plan <- make_install_plan(paste0("local::", pkg, "?nocache"), lib = libpath)
    Message
      i Getting 1 pkg with unknown size
      v Got foo 0.0.0.9000 (source)
    Code
      plan$metadata[[1]] <- c(Foo = "Bar", Foobar = "baz")
      plan$vignettes <- FALSE
      install_package_plan(plan, lib = libpath, num_workers = 1)
    Message
      i Building foo 0.0.0.9000
      v Built foo 0.0.0.9000
      v Installed foo 0.0.0.9000 (local)
      v Summary:   1 new

