
#' @importFrom crayon italic bold cyan silver bgRed white
#' @importFrom cli tree

pkgplan_draw_solution_tree <- function(self, private, pkgs, types,
                                       annotate) {

  assert_that(is.null(pkgs) || is_character(pkgs))
  types <- tolower(types %||% pkg_dep_types_hard())

  if (length(bad <- setdiff(types, tolower(pkg_dep_types())))) {
    stop("Unknown dependency type(s): ", paste(bad, collapse = ", "))
  }

  sol <- self$get_solution()$data
  pkgs <- pkgs %||% sol$package[sol$directpkg]

  data <- sol[, c("package", "deps")]

  deps <- lapply(sol$deps, function(x) x[tolower(x$type) %in% types, ])
  deps <- lapply(deps, "[[", "package")
  deps <- lapply(deps, intersect, data$package)
  data$deps <- deps

  data$label <- ifelse(
    sol$type %in% c("cran", "bioc", "standard", "installed"),
    data$package,
    sol$ref
  )
  data$label <- paste(data$label, silver(sol$version))
  data$label[sol$directpkg] <- italic(bold(cyan(data$label[sol$directpkg])))
  data$trimmed <- data$label

  if (annotate) {
    builder <- emoji("builder")
    data$label <- paste(data$label, annotate_tree(sol, builder = builder))
  }

  trees <- unlist(lapply(
    pkgs,
    function(p) c(tree(data, root = p, trim = TRUE), "")
  ))

  if (annotate) {
    key <- paste0(
      "Key: ",
      green(emoji("sparkles")), " new | ",
      green(emoji("rocket")), " update | ",
      green(emoji("hand")), " outdated |",
      green(emoji("dl")), " download | ",
      green(builder), " build | ",
      green(emoji("wrench")), " compile"
    )
    trees <- c(trees, key)
  }

  class(trees) <- c("tree", "character")
  trees
}

has_emoji <- function() {
  if (isTRUE(opt <- getOption("pkg.emoji"))) return(TRUE)
  if (identical(opt, FALSE)) return(FALSE)
  if (Sys.info()[["sysname"]] != "Darwin") return(FALSE)
  if (! isatty(stdout()) && Sys.getenv("RSTUDIO") == "") return(FALSE)
  TRUE
}

#' @importFrom crayon green

annotate_tree <- function(sol, builder = NULL) {
  new <- sol$lib_status == "new"
  dl  <- !is.na(sol$cache_status) & sol$cache_status == "miss"
  upd <- sol$lib_status == "update"

  builder <- builder %||% emoji("builder")

  green(paste0(
    ifelse(new, emoji("sparkles"), ""),
    ifelse(upd, emoji("rocket"), ""),
    ifelse(sol$lib_status == "no-update", emoji("hand"), ""),
    ifelse((new | upd) & sol$platform == "source", builder, ""),
    ifelse(
      (new | upd) & !is.na(sol$needscompilation) & sol$needscompilation,
      emoji("wrench"),
      ""
    ),
    ifelse(dl, paste(emoji("dl"), format_file_size(sol$filesize)), "")
  ))
}

#' @importFrom crayon silver
#' @importFrom prettyunits pretty_bytes

format_file_size <- function(x) {
  bts <- str_trim(pretty_bytes(x))
  silver(
    ifelse(is.na(x), "(unknown size)", paste0("(", bts, ")"))
  )
}

emoji <- function(what) {
  emo <- has_emoji()
  switch(
    what,
    "rocket"   = if (emo) "\U1F680" else "[new]",
    "sparkles" = if (emo) "\u2728"  else "[upd]",
    "hand"     = if (emo) "\u270B"  else "[old]",
    "dl"       = if (emo) " \u2B07"  else "[dl]",
    "builder"  = if (emo) emo_builder() else "[bld]",
    "wrench"   = if (emo) "\U1F527" else "[cmp]",
    ""
  )
}

emo_builder <- function(n = 1) {
  ppl <- c(
    "\U1F477",
    "\U1F477\u200D\u2640\uFE0F",
    "\U1F477\u200D\u2642\uFE0F",
    "\U1F477\U1F3FB",
    "\U1F477\U1F3FB\u200D\u2640\uFE0F",
    "\U1F477\U1F3FB\u200D\u2642\uFE0F",
    "\U1F477\U1F3FC",
    "\U1F477\U1F3FC\u200D\u2640\uFE0F",
    "\U1F477\U1F3FC\u200D\U2642\uFE0F",
    "\U1F477\U1F3FD",
    "\U1F477\U1F3FD\u200D\u2640\uFE0F",
    "\U1F477\U1F3FD\u200D\u2642\uFE0F",
    "\U1F477\U1F3FE",
    "\U1F477\U1F3FE\u200D\u2640\uFE0F",
    "\U1F477\U1F3FE\u200D\u2642\uFE0F",
    "\U1F477\U1F3FF",
    "\U1F477\U1F3FF\u200D\u2640\uFE0F",
    "\U1F477\U1F3FF\u200D\u2642\uFE0F"
  )
  paste(sample(ppl, n, replace = TRUE), collapse = "")
}
