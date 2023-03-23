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
      
      

# download_remote_git

    Code
      dir(file.path(prop$get_downloads()$fulltarget_tree, "cli"))
    Output
       [1] "DESCRIPTION"  "LICENSE"      "LICENSE.md"   "LICENSE.note" "Makefile"    
       [6] "NAMESPACE"    "NEWS.md"      "R"            "README.Rmd"   "README.md"   
      [11] "_pkgdown.yml" "cli.Rproj"    "codecov.yml"  "inst"         "man"         
      [16] "src"          "tests"        "tools"        "vignettes"   

