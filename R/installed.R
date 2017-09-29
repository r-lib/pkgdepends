
resolve_installed <- function(lib = .libPaths()[1]) {
  inst <- as_tibble(installed.packages(lib.loc = lib, noCache = TRUE))

  deps <- lapply(
    inst$Package,
    get_cran_deps,
    version = "",
    data = inst,
    dependencies = dependency_fields()
  )

  tibble(
    ref = inst$Package,
    direct = NA,
    status = "INSTALLED",
    package = inst$Package,
    version = inst$Version,
    platform = current_r_platform(),
    rversion = get_minor_r_version(inst$Built),
    repodir = NA_character_,
    sources = I(replicate(nrow(inst), character())),
    target = NA_character_,
    fulltarget = NA_character_,
    dependencies = deps
  )
}
