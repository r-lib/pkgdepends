# sysreqs_platforms

    Code
      print(sysreqs_platforms(), n = Inf)
    Output
      # A data frame: 19 x 9
         name                     os    id                  distribution version version_match update_command    install_command                  query_command
         <chr>                    <chr> <chr>               <chr>        <chr>   <chr>         <chr>             <chr>                            <chr>        
       1 Ubuntu Linux             linux ubuntu              ubuntu       *       <NA>          apt-get -y update apt-get -y install               dpkg-query   
       2 Debian Linux             linux debian              debian       *       <NA>          apt-get -y update apt-get -y install               dpkg-query   
       3 CentOS Linux             linux centos              centos       *       major         <NA>              yum install -y                   rpm          
       4 Rocky Linux              linux rocky               rockylinux   *       major         <NA>              dnf install -y                   rpm          
       5 Rocky Linux              linux rockylinux          rockylinux   *       major         <NA>              dnf install -y                   rpm          
       6 AlmaLinux                linux almalinux           rockylinux   *       major         <NA>              dnf install -y                   rpm          
       7 Red Hat Enterprise Linux linux rhel                redhat       6       major         <NA>              yum install -y                   rpm          
       8 Red Hat Enterprise Linux linux rhel                redhat       7       major         <NA>              yum install -y                   rpm          
       9 Red Hat Enterprise Linux linux rhel                redhat       *       major         <NA>              dnf install -y                   rpm          
      10 Red Hat Enterprise Linux linux redhat              redhat       6       major         <NA>              yum install -y                   rpm          
      11 Red Hat Enterprise Linux linux redhat              redhat       7       major         <NA>              yum install -y                   rpm          
      12 Red Hat Enterprise Linux linux redhat              redhat       *       major         <NA>              dnf install -y                   rpm          
      13 Fedora Linux             linux fedora              fedora       *       <NA>          <NA>              dnf install -y                   rpm          
      14 openSUSE Linux           linux opensuse            opensuse     *       <NA>          <NA>              zypper --non-interactive install rpm          
      15 openSUSE Linux           linux opensuse-leap       opensuse     *       <NA>          <NA>              zypper --non-interactive install rpm          
      16 openSUSE Linux           linux opensuse-tumbleweed opensuse     *       <NA>          <NA>              zypper --non-interactive install rpm          
      17 SUSE Linux Enterprise    linux sles                sle          *       <NA>          <NA>              zypper --non-interactive install rpm          
      18 SUSE Linux Enterprise    linux sle                 sle          *       <NA>          <NA>              zypper --non-interactive install rpm          
      19 Alpine Linux             linux alpine              alpine       *       minor         <NA>              apk add --no-cache               apk          

# sysreqs_db_list

    Code
      lst$packages
    Output
      [[1]]
      [1] "libcairo2-dev"
      
      [[2]]
      [1] "chromium"
      
      [[3]]
      [1] "libcurl4-openssl-dev"
      
    Code
      lst$pre_install
    Output
      [[1]]
      NULL
      
      [[2]]
      [1] "apt-get install -y software-properties-common"
      [2] "add-apt-repository -y ppa:xtradeb/apps"       
      [3] "apt-get update"                               
      
      [[3]]
      NULL
      
    Code
      lst$post_install
    Output
      [[1]]
      NULL
      
      [[2]]
      NULL
      
      [[3]]
      NULL
      

# sysreqs_db_match

    Code
      res
    Output
      [[1]]
      # A data frame: 2 x 5
        spec             sysreq  packages  pre_install post_install
        <chr>            <chr>   <list>    <list>      <list>      
      1 java and libcurl java    <chr [1]> <NULL>      <chr [1]>   
      2 java and libcurl libcurl <chr [1]> <NULL>      <NULL>      
      
      [[2]]
      # A data frame: 1 x 5
        spec                  sysreq  packages  pre_install post_install
        <chr>                 <chr>   <list>    <list>      <list>      
      1 openssl would be good openssl <chr [1]> <NULL>      <NULL>      
      
    Code
      lapply(res, "[[", "packages")
    Output
      [[1]]
      [[1]][[1]]
      [1] "default-jdk"
      
      [[1]][[2]]
      [1] "libcurl4-openssl-dev"
      
      
      [[2]]
      [[2]][[1]]
      [1] "libssl-dev"
      
      
    Code
      lapply(res, "[[", "post_install")
    Output
      [[1]]
      [[1]][[1]]
      [1] "R CMD javareconf"
      
      [[1]][[2]]
      NULL
      
      
      [[2]]
      [[2]][[1]]
      NULL
      
      

# sysreqs_install_plan

    Code
      res
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "ubuntu"
      
      $version
      [1] "22.04"
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install libcurl4-openssl-dev libssl-dev"
      
      $post_install
      character(0)
      
      $packages
      # A data frame: 2 x 5
        sysreq  packages  pre_install system_packages post_install
        <chr>   <list>    <list>      <list>          <list>      
      1 libcurl <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
      2 openssl <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
      
    Code
      res$packages$packages
    Output
      [[1]]
      [1] "curl"
      
      [[2]]
      [1] "curl"
      
    Code
      res$packages$system_packages
    Output
      [[1]]
      [1] "libcurl4-openssl-dev"
      
      [[2]]
      [1] "libssl-dev"
      

---

    Code
      res2
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "centos"
      
      $version
      [1] "7"
      
      $pre_install
      character(0)
      
      $install_scripts
      [1] "yum install -y make java-1.8.0-openjdk-devel"
      
      $post_install
      [1] "R CMD javareconf"
      
      $packages
      # A data frame: 2 x 5
        sysreq  packages  pre_install system_packages post_install
        <chr>   <list>    <list>      <list>          <list>      
      1 gnumake <chr [1]> <chr [0]>   <chr [1]>       <chr [0]>   
      2 java    <chr [1]> <chr [0]>   <chr [1]>       <chr [1]>   
      
    Code
      res2$packages$packages
    Output
      [[1]]
      [1] "rJava"
      
      [[2]]
      [1] "rJava"
      
    Code
      res2$packages$system_packages
    Output
      [[1]]
      [1] "make"
      
      [[2]]
      [1] "java-1.8.0-openjdk-devel"
      

# sysreqs_check_installed

    Code
      res
    Output
      system package       installed required by
      --------------       --        -----------
      gsfonts              v         magick     
      libcurl4-openssl-dev v         curl       
      libmagick++-dev      x         magick     
      libssl-dev           x         curl       

---

    Code
      res[]
    Output
      # A data frame: 4 x 5
        system_package       installed packages  pre_install post_install
        <chr>                <lgl>     <list>    <list>      <list>      
      1 gsfonts              TRUE      <chr [1]> <NULL>      <NULL>      
      2 libcurl4-openssl-dev TRUE      <chr [1]> <NULL>      <NULL>      
      3 libmagick++-dev      FALSE     <chr [1]> <NULL>      <NULL>      
      4 libssl-dev           FALSE     <chr [1]> <NULL>      <NULL>      

---

    Code
      sysreqs_check_installed()
    Output
      system package installed required by
      -------------- --        -----------

# async_parse_installed

    Code
      synchronize(async_parse_installed(library = .libPaths()[1], packages = c("foo",
        "bar", "baz")))
    Condition
      Warning in `async_parse_installed()`:
      Ignored 1 package that is not installed: baz.
    Output
      # A data frame: 2 x 1
        Package
        <chr>  
      1 foo    
      2 bar    

# parse_sysreqs_platform

    Code
      parse_sysreqs_platform("x86_64-pc-linux-gnu-ubuntu-22.04")
    Output
      # A data frame: 1 x 5
        cpu    vendor os    distribution version
        <chr>  <chr>  <chr> <chr>        <chr>  
      1 x86_64 pc     linux ubuntu       22.04  
    Code
      parse_sysreqs_platform("x86_64-pc-linux-musl-alpine-3.14.1")
    Output
      # A data frame: 1 x 5
        cpu    vendor os    distribution version
        <chr>  <chr>  <chr> <chr>        <chr>  
      1 x86_64 pc     linux alpine       3.14.1 
    Code
      parse_sysreqs_platform("x86_64-pc-linux-ubuntu-22.04")
    Output
      # A data frame: 1 x 5
        cpu    vendor os    distribution version
        <chr>  <chr>  <chr> <chr>        <chr>  
      1 x86_64 pc     linux ubuntu       22.04  
    Code
      parse_sysreqs_platform("ubuntu-22.04")
    Output
      # A data frame: 1 x 5
        cpu   vendor os    distribution version
        <chr> <chr>  <chr> <chr>        <chr>  
      1 <NA>  <NA>   linux ubuntu       22.04  
    Code
      parse_sysreqs_platform("aarch64-apple-darwin20")
    Output
      # A data frame: 1 x 5
        cpu     vendor os       distribution version
        <chr>   <chr>  <chr>    <chr>        <chr>  
      1 aarch64 apple  darwin20 <NA>         <NA>   
    Code
      parse_sysreqs_platform("i386+x86_64-w64-mingw32")
    Output
      # A data frame: 1 x 5
        cpu         vendor os      distribution version
        <chr>       <chr>  <chr>   <chr>        <chr>  
      1 i386+x86_64 w64    mingw32 <NA>         <NA>   
    Code
      parse_sysreqs_platform("x86_64-w64-mingw32")
    Output
      # A data frame: 1 x 5
        cpu    vendor os      distribution version
        <chr>  <chr>  <chr>   <chr>        <chr>  
      1 x86_64 w64    mingw32 <NA>         <NA>   
    Code
      parse_sysreqs_platform("ubuntu")
    Output
      # A data frame: 1 x 5
        cpu   vendor os    distribution version
        <chr> <chr>  <chr> <chr>        <chr>  
      1 <NA>  <NA>   linux ubuntu       <NA>   
    Code
      parse_sysreqs_platform("x86_64-px-linux-gnu-ubuntu-bar-baz")
    Output
      # A data frame: 1 x 5
        cpu    vendor os    distribution version
        <chr>  <chr>  <chr> <chr>        <chr>  
      1 x86_64 px     linux ubuntu       bar-baz
    Code
      parse_sysreqs_platform("ubuntu-22.04-libc++")
    Output
      # A data frame: 1 x 5
        cpu   vendor os    distribution version     
        <chr> <chr>  <chr> <chr>        <chr>       
      1 <NA>  <NA>   linux ubuntu       22.04-libc++

# query, post_install

    Code
      srq <- sysreqs_resolve("java", "ubuntu-22.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "ubuntu"
      
      $version
      [1] "22.04"
      
      $url
      [1] "<server>/__api__/repos/1/sysreqs?distribution=ubuntu&release=22.04"
      
      $total
      [1] 0.3333333
      
      $pre_install
      character(0)
      
      $install_scripts
      [1] "apt-get install -y default-jdk"
      
      $post_install
      [1] "R CMD javareconf"
      
      attr(,"class")
      [1] "pkg_sysreqs_result" "list"              

# pre_install

    Code
      srq <- sysreqs_resolve("this needs geos please", "ubuntu-16.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "ubuntu"
      
      $version
      [1] "16.04"
      
      $url
      [1] "<server>/__api__/repos/1/sysreqs?distribution=ubuntu&release=16.04"
      
      $total
      [1] 0.3333333
      
      $pre_install
      [1] "apt-get install -y software-properties-common"
      [2] "add-apt-repository -y ppa:ubuntugis/ppa"      
      [3] "apt-get update"                               
      
      $install_scripts
      [1] "apt-get install -y libgeos-dev"
      
      $post_install
      character(0)
      
      attr(,"class")
      [1] "pkg_sysreqs_result" "list"              

# multiple sysreqs

    Code
      srq <- sysreqs_resolve("java and also libcurl", "ubuntu-22.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "ubuntu"
      
      $version
      [1] "22.04"
      
      $url
      [1] "<server>/__api__/repos/1/sysreqs?distribution=ubuntu&release=22.04"
      
      $total
      [1] 0.3333333
      
      $pre_install
      character(0)
      
      $install_scripts
      [1] "apt-get install -y default-jdk"         
      [2] "apt-get install -y libcurl4-openssl-dev"
      
      $post_install
      [1] "R CMD javareconf"
      
      attr(,"class")
      [1] "pkg_sysreqs_result" "list"              

# error, unknown os

    Code
      sysreqs_resolve("java", "foobar-11")
    Condition
      Error:
      ! Failed to look up system requirements for OS foobar-11.
      i HTTP error 400 for <<server>/__api__/repos/1/sysreqs?distribution=foobar&release=11>.
      i Response: "{\"code\":14,\"error\":\"Unsupported system\",\"payload\":null}".

# sysreqs_install

    Code
      sysreqs_install(srq)
    Message
      i Installing system requirements
      i Executing `sh -c echo apt-get install -y libssl-dev libcurl4-openssl-dev`

---

    Code
      sysreqs_install(srq)
    Message
      i Installing system requirements
      i Executing `sh -c echo apt-get install -y default-jdk`
      i Executing `sh -c echo apt-get install -y libcurl4-openssl-dev`
      i Executing `sh -c echo R CMD javareconf`

---

    Code
      sysreqs_install(srq)
    Message
      i Installing system requirements
      i Executing `sh -c echo apt-get install -y default-jdk`
      apt-get install -y default-jdk
      i Executing `sh -c echo apt-get install -y libcurl4-openssl-dev`
      apt-get install -y libcurl4-openssl-dev
      i Executing `sh -c echo R CMD javareconf`
      R CMD javareconf

# compact_cmds

    Code
      compact_cmds(character())
    Output
      character(0)
    Code
      compact_cmds(c("apt-get install -y libssl-dev"))
    Output
      [1] "apt-get install -y libssl-dev"
    Code
      compact_cmds(c("apt-get install -y libssl-dev",
        "apt-get install -y libcurl4-openssl-dev"))
    Output
      [1] "apt-get install -y libssl-dev libcurl4-openssl-dev"

# highlight_sysreqs

    Code
      highlight_sysreqs(sq)
    Output
      [1] ""                                                          
      [2] " + v libfontconfig1-dev, v libfreetype6-dev"               
      [3] " + v libfreetype6-dev, x libfribidi-dev, x libharfbuzz-dev"
      [4] ""                                                          
    Code
      highlight_sysreqs(sq2)
    Output
      [1] ""                                                    
      [2] " + libfontconfig1-dev, libfreetype6-dev"             
      [3] " + libfreetype6-dev, libfribidi-dev, libharfbuzz-dev"
      [4] ""                                                    

---

    Code
      highlight_sysreqs(sq)
    Output
      [1] ""                                                                                                                                                                          
      [2] "\033[90m + \033[39m\033[36m\033[32m✔\033[36m libfontconfig1-dev\033[39m, \033[36m\033[32m✔\033[36m libfreetype6-dev\033[39m"                                               
      [3] "\033[90m + \033[39m\033[36m\033[32m✔\033[36m libfreetype6-dev\033[39m, \033[36m\033[31m✖\033[36m libfribidi-dev\033[39m, \033[36m\033[31m✖\033[36m libharfbuzz-dev\033[39m"
      [4] ""                                                                                                                                                                          
    Code
      highlight_sysreqs(sq2)
    Output
      [1] ""                                                                                                                    
      [2] "\033[90m + \033[39m\033[36mlibfontconfig1-dev\033[39m, \033[36mlibfreetype6-dev\033[39m"                             
      [3] "\033[90m + \033[39m\033[36mlibfreetype6-dev\033[39m, \033[36mlibfribidi-dev\033[39m, \033[36mlibharfbuzz-dev\033[39m"
      [4] ""                                                                                                                    

---

    Code
      highlight_sysreqs(sq)
    Output
      [1] ""                                                                                                                                                                          
      [2] "\033[90m + \033[39m\033[36mchrome (installer)\033[39m"                                                                                                                     
      [3] "\033[90m + \033[39m\033[36m\033[32m✔\033[36m libfreetype6-dev\033[39m, \033[36m\033[31m✖\033[36m libfribidi-dev\033[39m, \033[36m\033[31m✖\033[36m libharfbuzz-dev\033[39m"
      [4] ""                                                                                                                                                                          

