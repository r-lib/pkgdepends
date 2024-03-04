# parse_pkg_refs error on unknown type

    Code
      parse_pkg_refs(c("notgood::pkg", "good", "my_package"))
    Condition
      Error:
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

# gitlab

    Code
      parse_pkg_ref("gitlab::user/project")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "gitlab::user/project"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::user/project@ref")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "gitlab::user/project@ref"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::user/project/-/sub/dir")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "gitlab::user/project/-/sub/dir"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::user/project/-/sub/dir@ref")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "gitlab::user/project/-/sub/dir@ref"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::group/subgroup/project")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "gitlab::group/subgroup/project"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::group/subgroup/project@ref")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "gitlab::group/subgroup/project@ref"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::group/subgroup/project/-/sub/dir")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "gitlab::group/subgroup/project/-/sub/dir"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::group/subgroup/project/-/sub/dir@ref")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "gitlab::group/subgroup/project/-/sub/dir@ref"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("gitlab::https://acme.co/group/subgroup/project/-/sub/dir@ref")
    Output
      $package
      [1] "project"
      
      $protocol
      [1] "https"
      
      $host
      [1] "acme.co"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "gitlab::https://acme.co/group/subgroup/project/-/sub/dir@ref"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://acme.co/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::user/project")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "pkg=gitlab::user/project"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::user/project@ref")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "pkg=gitlab::user/project@ref"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::user/project/-/sub/dir")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "pkg=gitlab::user/project/-/sub/dir"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::user/project/-/sub/dir@ref")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "user"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "pkg=gitlab::user/project/-/sub/dir@ref"
      
      $path
      [1] "/user/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/user/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::group/subgroup/project")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "pkg=gitlab::group/subgroup/project"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::group/subgroup/project@ref")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] ""
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "pkg=gitlab::group/subgroup/project@ref"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::group/subgroup/project/-/sub/dir")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "HEAD"
      
      $ref
      [1] "pkg=gitlab::group/subgroup/project/-/sub/dir"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref("pkg=gitlab::group/subgroup/project/-/sub/dir@ref")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "gitlab.com"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "pkg=gitlab::group/subgroup/project/-/sub/dir@ref"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://gitlab.com/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             
    Code
      parse_pkg_ref(
        "pkg=gitlab::https://acme.co/group/subgroup/project/-/sub/dir@ref")
    Output
      $package
      [1] "pkg"
      
      $protocol
      [1] "https"
      
      $host
      [1] "acme.co"
      
      $projectpath
      [1] "group/subgroup"
      
      $project
      [1] "project"
      
      $subdir
      [1] "sub/dir"
      
      $commitish
      [1] "ref"
      
      $ref
      [1] "pkg=gitlab::https://acme.co/group/subgroup/project/-/sub/dir@ref"
      
      $path
      [1] "/group/subgroup/project"
      
      $dotgit
      [1] ""
      
      $url
      [1] "https://acme.co/group/subgroup/project.git"
      
      $type
      [1] "gitlab"
      
      $params
      character(0)
      
      attr(,"class")
      [1] "remote_ref_gitlab" "remote_ref"        "list"             

