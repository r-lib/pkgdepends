# install_packages: works with source packages

    Code
      plan <- make_install_plan(paste0("local::", pkg, "?nocache"), lib = libpath)
    Message
      i Getting 1 pkg with unknown size
      v Got foo 0.0.0.9000 (source)
    Code
      install_package_plan(plan, lib = libpath)
    Message
      i Building foo 0.0.0.9000
      v Built foo 0.0.0.9000
      v Installed foo 0.0.0.9000 (local)
      v Summary:   1 new

