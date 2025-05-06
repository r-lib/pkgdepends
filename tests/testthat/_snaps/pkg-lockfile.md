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

---

    Code
      plan$resolve()
    Condition
      Error:
      ! Cannot resolve an installation plan, it is already resolved.
    Code
      plan$async_resolve()
    Condition
      Error:
      ! Cannot resolve an installation plan, it is already resolved.

---

    Code
      plan$set_solve_policy()
    Condition
      Error:
      ! Cannot solve an installation plan, it is already solved.
    Code
      plan$solve()
    Condition
      Error:
      ! Cannot solve an installation plan, it is already solved.

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

---

    Code
      plan$resolve()
    Condition
      Error:
      ! Cannot resolve an installation plan, it is already resolved.
    Code
      plan$async_resolve()
    Condition
      Error:
      ! Cannot resolve an installation plan, it is already resolved.

---

    Code
      plan$set_solve_policy()
    Condition
      Error:
      ! Cannot solve an installation plan, it is already solved.
    Code
      plan$solve()
    Condition
      Error:
      ! Cannot solve an installation plan, it is already solved.

# install_sysreqs

    Code
      plan$show_solution()
    Message
      + curl   1.0.0 [bld][dl] (<size>) + libcurl4-openssl-dev, libssl-dev
    Code
      plan$show_sysreqs()
    Message
      * libcurl4-openssl-dev  - curl
      * libssl-dev            - curl

# update_sysreqs

    Code
      plan$show_sysreqs()
    Message
      + libcurl4-openssl-dev  - curl

# update_sysreqs with old lock file

    Code
      plan$show_sysreqs()
    Message
        

