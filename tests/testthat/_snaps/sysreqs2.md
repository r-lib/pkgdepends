# match

    Code
      sr1
    Output
      $os
      [1] "ubuntu"
      
      $os_release
      [1] "22.04"
      
      $url
      [1] NA
      
      $total
      [1] 0.05
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install default-jdk libcurl4-openssl-dev"
      
      $post_install
      [1] "R CMD javareconf"
      
      $records
      $records[[1]]
      $records[[1]]$packages
      [1] "default-jdk"
      
      $records[[1]]$pre_install
      NULL
      
      $records[[1]]$post_install
      [1] "R CMD javareconf"
      
      
      $records[[2]]
      $records[[2]]$packages
      [1] "libcurl4-openssl-dev"
      
      $records[[2]]$pre_install
      NULL
      
      $records[[2]]$post_install
      NULL
      
      
      

---

    Code
      sr2
    Output
      $os
      [1] "debian"
      
      $os_release
      [1] "unstable"
      
      $url
      [1] NA
      
      $total
      [1] 0.05
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install default-jdk libcurl4-openssl-dev"
      
      $post_install
      [1] "R CMD javareconf"
      
      $records
      $records[[1]]
      $records[[1]]$packages
      [1] "default-jdk"
      
      $records[[1]]$pre_install
      NULL
      
      $records[[1]]$post_install
      [1] "R CMD javareconf"
      
      
      $records[[2]]
      $records[[2]]$packages
      [1] "libcurl4-openssl-dev"
      
      $records[[2]]$pre_install
      NULL
      
      $records[[2]]$post_install
      NULL
      
      
      

# sysreqs2_command error

    Code
      sysreqs2_command("foobar", "2023")
    Error <rlib_error_3_0>
      ! Unknown OS. Don't know how to install system packages for foobar 2023

# do not run update if nothing to do

    Code
      sr1
    Output
      $os
      [1] "ubuntu"
      
      $os_release
      [1] "22.04"
      
      $url
      [1] NA
      
      $total
      [1] 0.05
      
      $pre_install
      character(0)
      
      $install_scripts
      NULL
      
      $post_install
      NULL
      
      $records
      list()
      

