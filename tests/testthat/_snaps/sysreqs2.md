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
    Condition
      Error:
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
      

# sysreqs_platforms

    Code
      sysreqs("ubuntu-20.04")
    Output
      [1] "apt-get -y install libxml2-dev"
    Code
      sysreqs("ubuntu-22.04")
    Output
      [1] "apt-get -y install libxml2-dev"
    Code
      sysreqs("ubuntu-24.04")
    Output
      [1] "apt-get -y install libxml2-dev"
    Code
      sysreqs("debian-11")
    Output
      [1] "apt-get -y install libxml2-dev"
    Code
      sysreqs("debian-12")
    Output
      [1] "apt-get -y install libxml2-dev"
    Code
      sysreqs("debian-unstable")
    Output
      [1] "apt-get -y install libxml2-dev"
    Code
      sysreqs("opensuse-15.4")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("opensuse-15.5")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("opensuse-15.6")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("opensuse-leap-15.4")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("opensuse-leap-15.5")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("opensuse-leap-15.6")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("opensuse-tumbleweed-20250509")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("centos-6")
    Output
      [1] "yum install -y libxml2-devel"
    Code
      sysreqs("centos-7")
    Output
      [1] "yum install -y libxml2-devel"
    Code
      sysreqs("centos-8")
    Output
      [1] "yum install -y libxml2-devel"
    Code
      sysreqs("rhel-7")
    Output
      [1] "yum install -y libxml2-devel"
    Code
      sysreqs("rhel-7.9")
    Output
      [1] "yum install -y libxml2-devel"
    Code
      sysreqs("rhel-8")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rhel-8.10")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rhel-9")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rhel-9.6")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("fedora-39")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("fedora-40")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("fedora-42")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("fedora-42")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("fedora-43")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("sles-15.4")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("sles-15.5")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("sles-15.6")
    Output
      [1] "zypper --non-interactive install libxml2-devel"
    Code
      sysreqs("almalinux-8.10")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("almalinux-9.6")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rocky-8.9")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rocky-9.3")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("alpine-3.19")
    Output
      [1] "apk add --no-cache libxml2-dev"
    Code
      sysreqs("alpine-3.20")
    Output
      [1] "apk add --no-cache libxml2-dev"
    Code
      sysreqs("alpine-3.21")
    Output
      [1] "apk add --no-cache libxml2-dev"
    Code
      sysreqs("alpine-3.edge")
    Output
      [1] "apk add --no-cache libxml2-dev"
    Code
      sysreqs("redhat-8")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("redhat-8.10")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rockylinux-8")
    Output
      [1] "dnf install -y libxml2-devel"
    Code
      sysreqs("rockylinux-8.9")
    Output
      [1] "dnf install -y libxml2-devel"

# canonize_sysreqs_platform

    Code
      canonize_sysreqs_platform("ubuntu-20.04")
    Output
      [1] "ubuntu-20.04"
    Code
      canonize_sysreqs_platform("ubuntu-22.04")
    Output
      [1] "ubuntu-22.04"
    Code
      canonize_sysreqs_platform("ubuntu-24.04")
    Output
      [1] "ubuntu-24.04"
    Code
      canonize_sysreqs_platform("debian-11")
    Output
      [1] "debian-11"
    Code
      canonize_sysreqs_platform("debian-12")
    Output
      [1] "debian-12"
    Code
      canonize_sysreqs_platform("debian-unstable")
    Output
      [1] "debian-unstable"
    Code
      canonize_sysreqs_platform("opensuse-15.4")
    Output
      [1] "opensuse-15.4"
    Code
      canonize_sysreqs_platform("opensuse-15.5")
    Output
      [1] "opensuse-15.5"
    Code
      canonize_sysreqs_platform("opensuse-15.6")
    Output
      [1] "opensuse-15.6"
    Code
      canonize_sysreqs_platform("opensuse-leap-15.4")
    Output
      [1] "opensuse-15.4"
    Code
      canonize_sysreqs_platform("opensuse-leap-15.5")
    Output
      [1] "opensuse-15.5"
    Code
      canonize_sysreqs_platform("opensuse-leap-15.6")
    Output
      [1] "opensuse-15.6"
    Code
      canonize_sysreqs_platform("opensuse-tumbleweed-20250509")
    Output
      [1] "opensuse-20250509"
    Code
      canonize_sysreqs_platform("centos-6")
    Output
      [1] "centos-6"
    Code
      canonize_sysreqs_platform("centos-7")
    Output
      [1] "centos-7"
    Code
      canonize_sysreqs_platform("centos-8")
    Output
      [1] "centos-8"
    Code
      canonize_sysreqs_platform("rhel-7")
    Output
      [1] "redhat-7"
    Code
      canonize_sysreqs_platform("rhel-7.9")
    Output
      [1] "redhat-7"
    Code
      canonize_sysreqs_platform("rhel-8")
    Output
      [1] "redhat-8"
    Code
      canonize_sysreqs_platform("rhel-8.10")
    Output
      [1] "redhat-8"
    Code
      canonize_sysreqs_platform("rhel-9")
    Output
      [1] "redhat-9"
    Code
      canonize_sysreqs_platform("rhel-9.6")
    Output
      [1] "redhat-9"
    Code
      canonize_sysreqs_platform("fedora-39")
    Output
      [1] "fedora-39"
    Code
      canonize_sysreqs_platform("fedora-40")
    Output
      [1] "fedora-40"
    Code
      canonize_sysreqs_platform("fedora-42")
    Output
      [1] "fedora-42"
    Code
      canonize_sysreqs_platform("fedora-42")
    Output
      [1] "fedora-42"
    Code
      canonize_sysreqs_platform("fedora-43")
    Output
      [1] "fedora-43"
    Code
      canonize_sysreqs_platform("sles-15.4")
    Output
      [1] "sle-15.4"
    Code
      canonize_sysreqs_platform("sles-15.5")
    Output
      [1] "sle-15.5"
    Code
      canonize_sysreqs_platform("sles-15.6")
    Output
      [1] "sle-15.6"
    Code
      canonize_sysreqs_platform("almalinux-8.10")
    Output
      [1] "rockylinux-8"
    Code
      canonize_sysreqs_platform("almalinux-9.6")
    Output
      [1] "rockylinux-9"
    Code
      canonize_sysreqs_platform("rocky-8.9")
    Output
      [1] "rockylinux-8"
    Code
      canonize_sysreqs_platform("rocky-9.3")
    Output
      [1] "rockylinux-9"
    Code
      canonize_sysreqs_platform("alpine-3.19")
    Output
      [1] "alpine-3.19"
    Code
      canonize_sysreqs_platform("alpine-3.20")
    Output
      [1] "alpine-3.20"
    Code
      canonize_sysreqs_platform("alpine-3.21")
    Output
      [1] "alpine-3.21"
    Code
      canonize_sysreqs_platform("alpine-3.19.1")
    Output
      [1] "alpine-3.19"
    Code
      canonize_sysreqs_platform("alpine-3.20.2")
    Output
      [1] "alpine-3.20"
    Code
      canonize_sysreqs_platform("alpine-3.21.3")
    Output
      [1] "alpine-3.21"
    Code
      canonize_sysreqs_platform("alpine-3.edge")
    Output
      [1] "alpine-3.edge"
    Code
      canonize_sysreqs_platform("redhat-8")
    Output
      [1] "redhat-8"
    Code
      canonize_sysreqs_platform("redhat-8.10")
    Output
      [1] "redhat-8"
    Code
      canonize_sysreqs_platform("rockylinux-8")
    Output
      [1] "rockylinux-8"
    Code
      canonize_sysreqs_platform("rockylinux-8.9")
    Output
      [1] "rockylinux-8"

