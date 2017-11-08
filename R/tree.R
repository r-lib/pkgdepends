
#' @importFrom crayon italic bold cyan silver bgRed white
#' @importFrom cli tree

remotes_draw_tree <- function(self, private, pkgs) {

  plan <- self$get_install_plan()
  pkgs <- pkgs %||% plan$package[plan$direct]

  data <- plan[, c("package", "dependencies")]
  data$label <- paste(
    data$package,
    silver(paste0("(", plan$version, ")"))
  )
  data$label[plan$direct] <- italic(bold(cyan(data$label[plan$direct])))

  trees <- unlist(lapply(pkgs, function(p) c(tree(data, root = p), "")))
  class(trees) <- c("tree", "character")
  trees
}
