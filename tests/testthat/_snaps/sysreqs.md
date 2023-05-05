# query, post_install

    Code
      srq <- sysreqs_resolve("java", "ubuntu", "22.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "ubuntu"
      
      $os_release
      [1] "22.04"
      
      $url
      [1] NA
      
      $total
      [1] 0.3333333
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install default-jdk"
      
      $post_install
      [1] "R CMD javareconf"
      
      $records
      $records[[1]]
      $records[[1]][[1]]
      $records[[1]][[1]]$sysreq
      [1] "java"
      
      $records[[1]][[1]]$packages
      [1] "default-jdk"
      
      $records[[1]][[1]]$pre_install
      NULL
      
      $records[[1]][[1]]$post_install
      [1] "R CMD javareconf"
      
      
      
      

# pre_install

    Code
      srq <- sysreqs_resolve("this needs geos please", "ubuntu", "16.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "ubuntu"
      
      $os_release
      [1] "16.04"
      
      $url
      [1] NA
      
      $total
      [1] 0.3333333
      
      $pre_install
      [1] "apt-get -y update"                            
      [2] "apt-get install -y software-properties-common"
      [3] "add-apt-repository -y ppa:ubuntugis/ppa"      
      [4] "apt-get update"                               
      
      $install_scripts
      [1] "apt-get -y install libgeos-dev"
      
      $post_install
      NULL
      
      $records
      $records[[1]]
      $records[[1]][[1]]
      $records[[1]][[1]]$sysreq
      [1] "geos"
      
      $records[[1]][[1]]$packages
      [1] "libgeos-dev"
      
      $records[[1]][[1]]$pre_install
      [1] "apt-get install -y software-properties-common"
      [2] "add-apt-repository -y ppa:ubuntugis/ppa"      
      [3] "apt-get update"                               
      
      $records[[1]][[1]]$post_install
      NULL
      
      
      
      

# multiple sysreqs

    Code
      srq <- sysreqs_resolve("java and also libcurl", "ubuntu", "22.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "ubuntu"
      
      $os_release
      [1] "22.04"
      
      $url
      [1] NA
      
      $total
      [1] 0.3333333
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install default-jdk libcurl4-openssl-dev"
      
      $post_install
      [1] "R CMD javareconf"
      
      $records
      $records[[1]]
      $records[[1]][[1]]
      $records[[1]][[1]]$sysreq
      [1] "java"
      
      $records[[1]][[1]]$packages
      [1] "default-jdk"
      
      $records[[1]][[1]]$pre_install
      NULL
      
      $records[[1]][[1]]$post_install
      [1] "R CMD javareconf"
      
      
      $records[[1]][[2]]
      $records[[1]][[2]]$sysreq
      [1] "libcurl"
      
      $records[[1]][[2]]$packages
      [1] "libcurl4-openssl-dev"
      
      $records[[1]][[2]]$pre_install
      NULL
      
      $records[[1]][[2]]$post_install
      NULL
      
      
      
      

# error, unknown os

    Code
      sysreqs_resolve("java", "foobar", "11")
    Error <async_rejected>
      ! Unknown OS. Don't know how to install system packages for foobar 11

# sysreqs_install

    Code
      sysreqs_install(srq)
    Message <cliMessage>
      i Installing system requirements
      i Executing `sh -c echo apt-get -y update`
      i Executing `sh -c echo apt-get -y install libcurl4-openssl-dev libssl-dev`

---

    Code
      sysreqs_install(srq)
    Message <cliMessage>
      i Installing system requirements
      i Executing `sh -c echo apt-get -y update`
      i Executing `sh -c echo apt-get -y install default-jdk libcurl4-openssl-dev`
      i Executing `sh -c echo R CMD javareconf`

---

    Code
      sysreqs_install(srq)
    Message <cliMessage>
      i Installing system requirements
      i Executing `sh -c echo apt-get -y update`
      apt-get -y update
      i Executing `sh -c echo apt-get -y install default-jdk libcurl4-openssl-dev`
      apt-get -y install default-jdk libcurl4-openssl-dev
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

