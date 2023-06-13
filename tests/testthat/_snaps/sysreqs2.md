# match

    Code
      sr1
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "ubuntu"
      
      $version
      [1] "22.04"
      
      $url
      [1] NA
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install default-jdk libcurl4-openssl-dev"
      
      $post_install
      [1] "R CMD javareconf"
      
      $packages
      [1] "default-jdk"          "libcurl4-openssl-dev"
      

---

    Code
      sr2
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "debian"
      
      $version
      [1] "unstable"
      
      $url
      [1] NA
      
      $pre_install
      [1] "apt-get -y update"
      
      $install_scripts
      [1] "apt-get -y install default-jdk libcurl4-openssl-dev"
      
      $post_install
      [1] "R CMD javareconf"
      
      $packages
      [1] "default-jdk"          "libcurl4-openssl-dev"
      

# sysreqs2_command error

    Code
      sysreqs2_command("foobar-2023")
    Error <rlib_error_3_0>
      ! Unknown OS. Don't know how to query or install system packages for foobar-2023.

# do not run update if nothing to do

    Code
      sr1
    Output
      $os
      [1] "linux"
      
      $distribution
      [1] "ubuntu"
      
      $version
      [1] "22.04"
      
      $url
      [1] NA
      
      $pre_install
      character(0)
      
      $install_scripts
      NULL
      
      $post_install
      NULL
      
      $packages
      NULL
      

