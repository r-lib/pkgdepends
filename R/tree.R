
#' @importFrom crayon italic bold cyan silver bgRed white
#' @importFrom cli tree

remotes_draw_solution_tree <- function(self, private, pkgs, types) {

  assert_that(is.null(pkgs) || is_character(pkgs))
  types <- tolower(types %||% dep_types_hard())

  if (length(bad <- setdiff(types, tolower(dep_types())))) {
    stop("Unknown dependency type(s): ", paste(bad, collapse = ", "))
  }

  sol <- self$get_solution()$data
  pkgs <- pkgs %||% sol$package[sol$direct]

  data <- sol[, c("package", "deps")]
  deps <- lapply(sol$deps, function(x) x[tolower(x$type) %in% types, ])
  deps <- lapply(deps, "[[", "package")
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
