# parse_pkg_refs error on unknown type

    Code
      parse_pkg_refs(c("notgood::pkg", "good", "my_package"))
    Error <rlib_error_3_0>
      ! Cannot parse package: my_package.
      i See `?pkgdepends::pkg_refs()` for supported package sources.

# explicit package names

    Code
      parse_pkg_ref("package=user/notpackage")
    Output
      $package
      [1] "package"
      
      $username
      [1] "user"
      
      $repo
      [1] "notpackage"
      
      $subdir
      [1] ""
      
      $commitish
      [1] ""
      
      $pull
      [1] ""
      
      $release
      [1] ""
      
      $ref
      [1] "package=user/notpackage"
      
      $type
      [1] "github"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_github" "remote_ref"        "list"             
    Code
      parse_pkg_ref("package=user/notpackage@tag")
    Output
      $package
      [1] "package"
      
      $username
      [1] "user"
      
      $repo
      [1] "notpackage"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "tag"
      
      $pull
      [1] ""
      
      $release
      [1] ""
      
      $ref
      [1] "package=user/notpackage@tag"
      
      $type
      [1] "github"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_github" "remote_ref"        "list"             
    Code
      parse_pkg_ref("package=github::user/notpackage@tag")
    Output
      $package
      [1] "package"
      
      $username
      [1] "user"
      
      $repo
      [1] "notpackage"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "tag"
      
      $pull
      [1] ""
      
      $release
      [1] ""
      
      $ref
      [1] "package=github::user/notpackage@tag"
      
      $type
      [1] "github"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_github" "remote_ref"        "list"             
    Code
      parse_pkg_ref("package=local::/abs/path")
    Output
      $package
      [1] "package"
      
      $path
      [1] "/abs/path"
      
      $ref
      [1] "package=local::/abs/path"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
    Code
      parse_pkg_ref("package=local::rel/path")
    Output
      $package
      [1] "package"
      
      $path
      [1] "rel/path"
      
      $ref
      [1] "package=local::rel/path"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
    Code
      parse_pkg_ref("package=local::~/home/path")
    Output
      $package
      [1] "package"
      
      $path
      [1] "~/home/path"
      
      $ref
      [1] "package=local::~/home/path"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
    Code
      parse_pkg_ref("package=/abs/path")
    Output
      $package
      [1] "package"
      
      $path
      [1] "/abs/path"
      
      $ref
      [1] "package=local::/abs/path"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
    Code
      parse_pkg_ref("package=./rel/path")
    Output
      $package
      [1] "package"
      
      $path
      [1] "./rel/path"
      
      $ref
      [1] "package=local::./rel/path"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
    Code
      parse_pkg_ref("package=~/home/path")
    Output
      $package
      [1] "package"
      
      $path
      [1] "~/home/path"
      
      $ref
      [1] "package=local::~/home/path"
      
      $type
      [1] "local"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_local" "remote_ref"       "list"            
    Code
      parse_pkg_ref("package=url::https://example.com/p1.tar.gz")
    Output
      $package
      [1] "package"
      
      $url
      [1] "https://example.com/p1.tar.gz"
      
      $ref
      [1] "package=url::https://example.com/p1.tar.gz"
      
      $type
      [1] "url"
      
      $hash
      [1] "abc44e36b1d7392a347beee661d5342b"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_url" "remote_ref"     "list"          

