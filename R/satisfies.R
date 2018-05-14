
satisfies_remote <- function(resolution, candidate, config,
                             remote_types = NULL, ...) {
  remote_types <- c(default_remote_types(), remote_types)
  sat <- remote_types[[resolution$type]]$satisfy
  if (is.null(sat)) return(resolution$ref == candidate$ref)

  sat(resolution, candidate, config, ...)
}
