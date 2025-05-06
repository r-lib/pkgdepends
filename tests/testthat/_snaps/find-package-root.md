# find_package_root errors

    Code
      find_package_root("file1716b354e464b")
    Condition
      Error:
      ! Path does not exist: file1716b354e464b

---

    Code
      find_package_root("/")
    Condition
      Error:
      ! Could not find R package in `/` or its parent directories.

