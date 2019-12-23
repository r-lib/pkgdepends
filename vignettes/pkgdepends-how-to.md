---
title: "pkgdepends HOWTO"
output:
  rmarkdown::html_vignette:
    keep_md: true
    toc: true
    toc_depth: 2
vignette: >
  %\VignetteIndexEntry{pkgdepends how tos}
  %\VignetteEngine{pkgdepends::lazyrmd}
  %\VignetteEncoding{UTF-8}
---


  
# Dependencies

## How to list all dependencies of a CRAN/Bioconductor package?


```asciicast
library(pkgdepends)
prop <- new_pkg_deps("ggplot2")
prop$solve()
prop$get_solution()$data
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-2.svg" width="100%" />

You can also draw a dependency tree:


```asciicast
prop$draw()
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-3.svg" width="100%" />

## How to list all dependencies of a GitHub package?


```asciicast
library(pkgdepends)
prop <- new_pkg_deps("tidyverse/ggplot2")
prop$solve()
prop$get_solution()$data
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-4.svg" width="100%" />

## How to list all dependencies of a local package?


```asciicast
library(pkgdepends)
prop <- new_pkg_deps("local::.")
prop$solve()
prop$get_solution()$data
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-5.svg" width="100%" />

# Downloads

## How to download a package and all of its dependencies?


```asciicast
library(pkgdepends)
target_dir <- tempfile()
dir.create(target_dir)
prop <- new_pkg_download_proposal("ggplot2", config = list(cache_dir = target_dir))
prop$resolve()
prop$download()
prop$get_downloads()
dir(target_dir)
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-6.svg" width="100%" />

# Installation

## How to install a package into a new library?


```asciicast
library(pkgdepends)
dir.create(new_lib <- tempfile())
prop <- new_pkg_installation_proposal("pkgconfig", config = list(library = new_lib))
prop$solve()
prop$download()
prop$install()
lib_status(new_lib)
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-7.svg" width="100%" />

## How to update a package?

Install an older version first.


```asciicast
library(pkgdepends)
dir.create(new_lib <- tempfile())
config <- list(library = new_lib)
prop <- new_pkg_installation_proposal("cran/pkgconfig@2.0.2", config = config)
prop$solve()
prop$download()
prop$install()
lib_status(new_lib)
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-8.svg" width="100%" />

Now update.


```asciicast
library(pkgdepends)
prop2 <- new_pkg_installation_proposal("pkgconfig", config = config)
prop2$set_solve_policy("upgrade")
prop2$solve()
prop2$download()
prop2$install()
lib_status(new_lib)
```


<img src="pkgdepends-how-to_files/figure-html//unnamed-chunk-9.svg" width="100%" />
