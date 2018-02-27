
#' @export

make_install_plan <- function(pkgs, library = .libPaths()[[1]],
                              config = list()) {
  
  rem <- remotes$new(pkgs, library = library, config = config)

  res <- rem$resolve()

  ## TODO: handle resolution errors here

  sol <- rem$solve()

  ## TODO: handle solutions errors here

  dls <- rem$download_solution()

  ## TODO: handle download errors here

  rem$get_install_plan()
}
