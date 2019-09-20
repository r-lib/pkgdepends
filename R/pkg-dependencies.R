
#' @export

new_pkg_deps <- function(refs) {
  pkg_deps$new(refs)
}

#' R6 class for package dependency lookup
#'
#' Create a `pkg_deps` object to look up dependencies of R packages.
#'
#' @includeRmd tools/doc/pkg-dependencies.Rmd
#' @name pkg_deps
NULL

#' @export

pkg_deps <- R6::R6Class(
  "pkg_deps",
  public = list(
    initialize = function(refs, config = list(), remote_types = NULL)
      pkgdeps_init(self, private, refs, config, remote_types),
    get_refs = function() private$plan$get_refs(),
    get_config = function() private$plan$get_config(),
    async_resolve = function()
      pkgdeps_async_resolve(self, private),
    resolve = function()
      pkgdeps_resolve(self, private),
    get_resolution = function()
      pkgdeps_get_resolution(self, private),

    solve = function()
      pkgdeps_solve(self, private),
    get_solution = function()
      pkgdeps_get_solution(self, private),
    draw = function()
      pkgdeps_draw(self, private),

    format = function(...) pkgdeps_format(self, private, ...),
    print = function(...) pkgdeps_print(self, private, ...)
  ),

  private = list(
    plan = NULL,
     library = NULL
  )
)

pkgdeps_init <- function(self, private, refs, config, remote_types) {
  private$library <- tempfile()
  dir.create(private$library)
  private$plan <- pkg_plan$new(
    refs,
    config,
    library = private$library,
    remote_types
  )
}

pkgdeps_async_resolve <- function(self, private) {
  private$plan$async_resolve()
}

pkgdeps_resolve <- function(self, private) {
  invisible(private$plan$resolve())
}

pkgdeps_get_resolution <- function(self, private) {
  private$plan$get_resolution()
}

pkgdeps_solve <- function(self, private) {
  invisible(private$plan$solve(policy = "lazy"))
}

pkgdeps_get_solution <- function(self, private) {
  private$plan$get_solution()
}

pkgdeps_draw <- function(self, private) {
  private$plan$draw_solution_tree()
}

pkgdeps_format <- function(self, private, ...) {
  refs <- private$plan$get_refs()

  has_res <- private$plan$has_resolution()
  res <- if (has_res) private$plan$get_resolution()
  res_err <- has_res && any(res$status != "OK")

  has_sol <- private$plan$has_solution()
  sol <- if (has_sol) private$plan$get_solution()
  sol_err <- has_sol && sol$status != "OK"

  deps <- if (has_res) length(unique(res$package[!res$direct]))

  c("<pkg_dependencies>",
    "+ refs:", paste0("  - ", refs),
    if (has_res) paste0("+ has resolution (+", deps, " dependencies)"),
    if (res_err) "x has resolution errors",
    if (has_sol) "+ has solution",
    if (sol_err) "x has solution errors",
    if (!has_res) "(use `$resolve()` to resolve dependencies)",
    if (has_res) "(use `$get_resolution()` to see resolution results)",
    if (!has_sol) "(use `$solve()` to solve dependencies)",
    if (has_sol) "(use `$get_solution()` to see solution results)",
    if (has_sol && !sol_err) "(use `$draw()` to draw the dependency tree)"
  )
}

pkgdeps_print <- function(self, private, ...) {
  cat(self$format(...), sep = "\n")
}
