# parse_remote_git

    Code
      parse_remote_git("git::https://github.com/r-lib/cli")
    Output
      [[1]]
      [[1]]$package
      [1] "cli"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "HEAD"
      
      [[1]]$ref
      [1] "git::https://github.com/r-lib/cli"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ""
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli"
      
      

---

    Code
      parse_remote_git("git::https://github.com/r-lib/cli.git")
    Output
      [[1]]
      [[1]]$package
      [1] "cli"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "HEAD"
      
      [[1]]$ref
      [1] "git::https://github.com/r-lib/cli.git"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ".git"
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli.git"
      
      

---

    Code
      parse_remote_git("git::https://github.com/r-lib/cli@branch")
    Output
      [[1]]
      [[1]]$package
      [1] "cli"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "branch"
      
      [[1]]$ref
      [1] "git::https://github.com/r-lib/cli@branch"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ""
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli"
      
      

---

    Code
      parse_remote_git("git::https://github.com/r-lib/cli.git@branch")
    Output
      [[1]]
      [[1]]$package
      [1] "cli"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "branch"
      
      [[1]]$ref
      [1] "git::https://github.com/r-lib/cli.git@branch"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ".git"
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli.git"
      
      

---

    Code
      parse_remote_git("pkg=git::https://github.com/r-lib/cli")
    Output
      [[1]]
      [[1]]$package
      [1] "pkg"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "HEAD"
      
      [[1]]$ref
      [1] "pkg=git::https://github.com/r-lib/cli"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ""
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli"
      
      

---

    Code
      parse_remote_git("pkg=git::https://github.com/r-lib/cli.git")
    Output
      [[1]]
      [[1]]$package
      [1] "pkg"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "HEAD"
      
      [[1]]$ref
      [1] "pkg=git::https://github.com/r-lib/cli.git"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ".git"
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli.git"
      
      

---

    Code
      parse_remote_git("pkg=git::https://github.com/r-lib/cli@branch")
    Output
      [[1]]
      [[1]]$package
      [1] "pkg"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "branch"
      
      [[1]]$ref
      [1] "pkg=git::https://github.com/r-lib/cli@branch"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ""
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli"
      
      

---

    Code
      parse_remote_git("pkg=git::https://github.com/r-lib/cli.git@branch")
    Output
      [[1]]
      [[1]]$package
      [1] "pkg"
      
      [[1]]$protocol
      [1] "https"
      
      [[1]]$host
      [1] "github.com"
      
      [[1]]$path
      [1] "/r-lib/"
      
      [[1]]$repo
      [1] "cli"
      
      [[1]]$commitish
      [1] "branch"
      
      [[1]]$ref
      [1] "pkg=git::https://github.com/r-lib/cli.git@branch"
      
      [[1]]$type
      [1] "git"
      
      [[1]]$dotgit
      [1] ".git"
      
      [[1]]$url
      [1] "https://github.com/r-lib/cli.git"
      
      

---

    Code
      parse_pkg_refs(c("git::https://github.com/cran/falsy.git",
        "git::https://github.com/cran/falsy2.git"))
    Output
      [[1]]
      $package
      [1] "falsy"
      
      $protocol
      [1] "https"
      
      $host
      [1] "github.com"
      
      $path
      [1] "/cran/"
      
      $repo
      [1] "falsy"
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "git::https://github.com/cran/falsy.git"
      
      $type
      [1] "git"
      
      $dotgit
      [1] ".git"
      
      $url
      [1] "https://github.com/cran/falsy.git"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_git" "remote_ref"     "list"          
      
      [[2]]
      $package
      [1] "falsy2"
      
      $protocol
      [1] "https"
      
      $host
      [1] "github.com"
      
      $path
      [1] "/cran/"
      
      $repo
      [1] "falsy2"
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "git::https://github.com/cran/falsy2.git"
      
      $type
      [1] "git"
      
      $dotgit
      [1] ".git"
      
      $url
      [1] "https://github.com/cran/falsy2.git"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_git" "remote_ref"     "list"          
      

# download_remote_git

    Code
      dir(file.path(prop$get_downloads()$fulltarget_tree, "cli"))
    Output
       [1] "DESCRIPTION"  "LICENSE"      "LICENSE.md"   "LICENSE.note" "Makefile"    
       [6] "NAMESPACE"    "NEWS.md"      "R"            "README.Rmd"   "README.md"   
      [11] "_pkgdown.yml" "cli.Rproj"    "codecov.yml"  "inst"         "man"         
      [16] "src"          "tests"        "tools"        "vignettes"   

