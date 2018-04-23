
#' @importFrom crayon italic bold cyan silver bgRed white
#' @importFrom cli tree

remotes_draw_tree <- function(self, private, pkgs) {

  assert_that(is.null(pkgs) || is_character(pkgs))

  sol <- self$get_solution()$data
  pkgs <- pkgs %||% sol$package[sol$direct]

  data <- sol[, c("package", "deps")]
  deps <- lapply(sol$deps, "[[", "package")
  deps <- lapply(deps, intersect, data$package)
  data$deps <- deps
  data$label <- paste(
    data$package,
    silver(paste0("(", sol$version, ")"))
  )
  data$label[sol$direct] <- italic(bold(cyan(data$label[sol$direct])))

  trees <- unlist(lapply(pkgs, function(p) c(tree(data, root = p), "")))
  class(trees) <- c("tree", "character")
  trees
}
