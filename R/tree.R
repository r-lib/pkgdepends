
#' @importFrom crayon italic bold cyan silver bgRed white
#' @importFrom cli tree

remotes_draw_tree <- function(self, private, pkgs) {

  assert_that(is.null(pkgs) || is_character(pkgs))

  sol <- self$get_solution()$data$data
  pkgs <- pkgs %||% sol$package[sol$direct]

  data <- sol[, c("package", "dependencies")]
  deps <- lapply(sol$dependencies, "[[", "package")
  deps <- lapply(deps, setdiff, y = "R")
  data$dependencies <- deps
  data$label <- paste(
    data$package,
    silver(paste0("(", sol$version, ")"))
  )
  data$label[sol$direct] <- italic(bold(cyan(data$label[sol$direct])))

  trees <- unlist(lapply(pkgs, function(p) c(tree(data, root = p), "")))
  class(trees) <- c("tree", "character")
  trees
}
