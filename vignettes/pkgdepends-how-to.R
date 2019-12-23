## ---- include = FALSE, cache = FALSE--------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  cache = TRUE
)
# Turn on ANSI colors
options(
    crayon.enabled = TRUE,
    crayon.colors = 256)
crayon::num_colors(forget = TRUE)
asciicast::init_knitr_engine(
  startup = quote({
    set.seed(1) }),
  echo = TRUE,
  echo_input = FALSE)

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
prop <- new_pkg_deps("ggplot2")
prop$solve()
prop$get_solution()$data

## ----engine="asciicast"---------------------------------------------------------------------------
prop$draw()

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
prop <- new_pkg_deps("tidyverse/ggplot2")
prop$solve()
prop$get_solution()$data

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
prop <- new_pkg_deps("local::.")
prop$solve()
prop$get_solution()$data

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
target_dir <- tempfile()
dir.create(target_dir)
prop <- new_pkg_download_proposal("ggplot2", config = list(cache_dir = target_dir))
prop$resolve()
prop$download()
prop$get_downloads()
dir(target_dir)

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
dir.create(new_lib <- tempfile())
prop <- new_pkg_installation_proposal("pkgconfig", config = list(library = new_lib))
prop$solve()
prop$download()
prop$install()
lib_status(new_lib)

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
dir.create(new_lib <- tempfile())
config <- list(library = new_lib)
prop <- new_pkg_installation_proposal("cran/pkgconfig@2.0.2", config = config)
prop$solve()
prop$download()
prop$install()
lib_status(new_lib)

## ----engine="asciicast"---------------------------------------------------------------------------
library(pkgdepends)
prop2 <- new_pkg_installation_proposal("pkgconfig", config = config)
prop2$set_solve_policy("upgrade")
prop2$solve()
prop2$download()
prop2$install()
lib_status(new_lib)

