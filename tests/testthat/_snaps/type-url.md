# url remote basics

    Code
      r$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz
      + constraints (1):
        - select pkg1 exactly once
      + solution:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz

# url to an tree (not a source package), from a zip file

    Code
      r$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - url::http://127.0.0.1:<port>/repos/r-lib/pak/zipball/111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a
      + constraints (1):
        - select pak exactly once
      + solution:
        - url::http://127.0.0.1:<port>/repos/r-lib/pak/zipball/111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a

# satisfy

    Code
      satisfy_remote_url(list(package = "package"), list(package = "other"))
    Output
      [1] FALSE
      attr(,"reason")
      [1] "Package names differ"

---

    Code
      satisfy_remote_url(list(package = "package", params = list(c(reinstall = ""))),
      list(type = "installed", package = "package"))
    Output
      [1] FALSE
      attr(,"reason")
      [1] "Re-install requested"

---

    Code
      satisfy_remote_url(list(package = "package", metadata = list(list(RemoteEtag = "myetag"))),
      list(type = "installed", package = "package"))
    Output
      [1] FALSE
      attr(,"reason")
      [1] "Installed URL etag mismatch"

---

    Code
      satisfy_remote_url(list(package = "package", metadata = list(list(RemoteEtag = "myetag"))),
      list(type = "installed", package = "package", extra = list(list(remoteetag = "otheretag"))))
    Output
      [1] FALSE
      attr(,"reason")
      [1] "Installed URL etag mismatch"

---

    Code
      satisfy_remote_url(list(package = "package", metadata = list(list(RemoteEtag = "myetag"))),
      list(type = "installed", package = "package", extra = list(list(remoteetag = "myetag"))))
    Output
      [1] TRUE

---

    Code
      satisfy_remote_url(list(package = "package", ref = "https://myurl"), list(type = "url",
        package = "package", ref = "https://anotherurl"))
    Output
      [1] FALSE
      attr(,"reason")
      [1] "URL mismatch"

---

    Code
      satisfy_remote_url(list(package = "package", ref = "https://myurl"), list(type = "url",
        package = "package", ref = "https://myurl"))
    Output
      [1] TRUE

---

    Code
      satisfy_remote_url(list(package = "package", ref = "https://myurl"), list(type = "local",
        package = "package"))
    Output
      [1] FALSE
      attr(,"reason")
      [1] "Repo type mismatch"

# nocache

    Code
      r$get_solution()
    Output
      <pkg_solution>
      + result: OK
      + refs:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz
      + constraints (1):
        - select pkg1 exactly once
      + solution:
        - url::http://127.0.0.1:<port>//src/contrib/Archive/pkg1/pkg1_0.9.0.tar.gz

# bad archive file (multiple directories)

    Code
      get_pkg_dir_from_archive_dir(tmp)
    Condition
      Error:
      ! Package archive at '<tempdir>/<tempfile>' should contain exactly one directory.
      i It has 2 files/directories: 'pkg1' and 'pkg2'

