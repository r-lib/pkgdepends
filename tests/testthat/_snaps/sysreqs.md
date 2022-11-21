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
      srq <- sysreqs_resolve("this needs geos please", "ubuntu", "16.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "ubuntu"
      
      $os_release
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
      srq <- sysreqs_resolve("java and also libcurl", "ubuntu", "22.04")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "ubuntu"
      
      $os_release
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

# system is detected

    Code
      srq <- sysreqs_resolve("java and also libcurl")
      srq$total <- 1 / 3
      srq
    Output
      $os
      [1] "ubuntu"
      
      $os_release
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
      sysreqs_resolve("java", "debian", "11")
    Error <async_rejected>
      ! Failed to look up system requirements for OS debian 11.
      i HTTP error 400 for <<server>/__api__/repos/1/sysreqs?distribution=debian&release=11>.
      i Response: "{\"code\":14,\"error\":\"Unsupported system\",\"payload\":null}".

# sysreqs_install

    Code
      sysreqs_install(srq)
    Message <cliMessage>
      i Installing system requirements
      i Executing `sh -c echo apt-get install -y default-jdk`
      i Executing `sh -c echo apt-get install -y libcurl4-openssl-dev`
      i Executing `sh -c echo R CMD javareconf`

---

    Code
      sysreqs_install(srq)
    Message <cliMessage>
      i Installing system requirements
      i Executing `sh -c echo apt-get install -y default-jdk`
      apt-get install -y default-jdk
      i Executing `sh -c echo apt-get install -y libcurl4-openssl-dev`
      apt-get install -y libcurl4-openssl-dev
      i Executing `sh -c echo R CMD javareconf`
      R CMD javareconf

# detect_linux

    Code
      detect_linux()
    Output
      $distribution
      [1] "ubuntu"
      
      $release
      [1] "22.04"
      

---

    Code
      detect_linux()
    Output
      $distribution
      [1] "unknown"
      
      $release
      [1] "unknown"
      

