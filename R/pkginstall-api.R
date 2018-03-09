
#' Create a package installation plan
#'
#' TODO
#'
#' @param pkgs Package names (or remote references) to install.
#' @param library Package library directory to install to. The packages
#'   already installed there are considered for the install plan.
#' @param config Configuration options, see [remotes].
#' @return The install plan, a data frame.
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
