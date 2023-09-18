# new_pkg_installation_plan

    Code
      plan
    Output
      <pkg_installation_plan>
      + refs:
        - pkg3
      + has solution
      (use `$update()` to update the plan for an updated library)
      (use `$show_solution()` to see the packages to install
      (use `$get_solution()` to see the full solution results)
      (use `$draw()` to draw the dependency tree)
      (use `$download()` to download packages)

# sysreqs

    Code
      plan
    Output
      <pkg_installation_plan>
      + refs:
        - pkg3
      + has solution
      (use `$update()` to update the plan for an updated library)
      (use `$show_solution()` to see the packages to install
      (use `$get_solution()` to see the full solution results)
      (use `$draw()` to draw the dependency tree)
      (use `$download()` to download packages)

# install_sysreqs

    Code
      plan$show_solution()
    Message <cliMessage>
      + curl   1.0.0 [bld][dl] (<size>) + libcurl4-openssl-dev, libssl-dev
    Code
      plan$show_sysreqs()
    Message <cliMessage>
      * libcurl4-openssl-dev  - curl
      * libssl-dev            - curl

# update_sysreqs

    Code
      plan$show_sysreqs()
    Message <cliMessage>
      + libcurl4-openssl-dev  - curl

# update_sysreqs with old lock file

    Code
      plan$show_sysreqs()
    Message <cliMessage>
        

