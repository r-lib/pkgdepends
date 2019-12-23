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
dir.create(new_lib <- tempfile())
prop <- new_pkg_installation_proposal("pak", config = list(library = new_lib))
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

