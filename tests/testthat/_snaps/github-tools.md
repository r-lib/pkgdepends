# type_github_get_data, sha, description

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak")))
    Output
      $sha
      [1] "111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@HEAD")))
    Output
      $sha
      [1] "111ef906acb58fe406370f7bc0a72cac55dbbb231ea687494c25742ca521255a"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@v0.1.2")))
    Output
      $sha
      [1] "a503fe843f11c279864f29d58137f8de319d115b239ce48ccc15406306019480"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@*release")))
    Output
      $sha
      [1] "b001d6ddeab1589ad367b62baabbeeb2af3b0ebac2e61d239df660c1d63e3232"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@e65de1e9630d")))
    Output
      $sha
      [1] "e65de1e9630dbfcaf1044718b742bf806486b107239ce48ccc15406306019480"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref(
        "r-lib/pak@e65de1e9630dbfcaf1044718b742bf806486b107")))
    Output
      $sha
      [1] "e65de1e9630dbfcaf1044718b742bf806486b107239ce48ccc15406306019480"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak#90")))
    Output
      $sha
      [1] "b001d6ddeab1589ad367b62baabbeeb2af3b0ebac2e61d239df660c1d63e3232"
      
      $description
      Package: pak
      Version: 1.0.0
      

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref(
        "wesm/feather/R@ec40c1eae1ac83b86fc41bb2f5cd916152d19015")))
    Output
      $sha
      [1] "ec40c1eae1ac83b86fc41bb2f5cd916152d19015649c3d209f2c08115dd993b1"
      
      $description
      Package: feather
      Version: 1.0.0
      

# type_github_get_data, no such user

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib-xxx-xxx/pak")))
    Condition
      Error:
      ! Can't find GitHub repo r-lib-xxx-xxx/pak.

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib-xxx-xxx/pak#90")))
    Condition
      Error:
      ! Can't find GitHub repo r-lib-xxx-xxx/pak.

# type_github_get_data, no such repo

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx")))
    Condition
      Error:
      ! Can't find GitHub repo r-lib/pak-xxx-xxx.

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx#90")))
    Condition
      Error:
      ! Can't find GitHub repo r-lib/pak-xxx-xxx.

# github_query, invalid PAT

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx")))
    Condition
      Error:
      ! Bad GitHub credentials, make sure that your GitHub token is valid.
      Caused by error:
      ! Unauthorized (HTTP 401).

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak-xxx-xxx#90")))
    Condition
      Error:
      ! Bad GitHub credentials, make sure that your GitHub token is valid.
      Caused by error:
      ! Unauthorized (HTTP 401).

# github_query, no internet

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak")))
    Condition
      Error:
      ! Cannot query GitHub, are you offline?

# github_query, access denied

    Code
      synchronise(type_github_get_data(parse_pkg_ref("gaborcsardi/secret-test")))
    Condition
      Error:
      ! Can't find GitHub repo gaborcsardi/secret-test.

# cannot find R package on GitHub, no DESCRIPTION

    Code
      synchronise(type_github_get_data(parse_pkg_ref("tidyverse/tidyverse.org")))
    Condition
      Error:
      ! Can't find R package in GitHub repo tidyverse/tidyverse.org

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/cranyon/R")))
    Condition
      Error:
      ! Can't find GitHub repo r-lib/cranyon.

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/crayon/R#79")))
    Condition
      Error:
      ! Can't find R package in GitHub repo r-lib/crayon in directory 'R'

# cannot parse DESCRIPTION on GH

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/bad@main")))
    Condition
      Error:
      ! Can't parse DESCRIPTION file in GitHub repo r-lib/bad
      Caused by error:
      ! Line starting 'this is not ...' is malformed!

---

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/bad#100")))
    Condition
      Error:
      ! Can't parse DESCRIPTION file in GitHub repo r-lib/bad
      Caused by error:
      ! Line starting 'this is not ...' is malformed!

---

    Code
      synchronize(type_github_get_data(parse_pkg_ref("r-lib/bad/bin@main")))
    Condition
      Error:
      ! Can't parse DESCRIPTION file in GitHub repo r-lib/bad, in directory `bin`

# http error

    Code
      synchronise(type_github_get_data(parse_pkg_ref("foo/bar")))
    Condition
      Error:
      ! GitHub HTTP error
      Caused by error:
      ! Not Found (HTTP 404).

# no such PR error

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak#89")))
    Condition
      Error:
      ! Can't find PR #89 in GitHub repo r-lib/pak

# no such ref error

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/pak@bad-ref-no-no-no")))
    Condition
      Error:
      ! Can't find reference @bad-ref-no-no-no in GitHub repo r-lib/pak.

# no release error

    Code
      synchronise(type_github_get_data(parse_pkg_ref("r-lib/bad@*release")))
    Condition
      Error:
      ! Can't find any release in GitHub repo r-lib/bad.

# builtin token messages once per session

    Code
      type_github_builtin_token()
    Message
      ! Using bundled GitHub PAT. Please add your own PAT using `gitcreds::gitcreds_set()`.
    Output
      [1] "builtin-token"

---

    Code
      type_github_builtin_token()
    Output
      [1] "builtin-token"

# CI specific token is picked up if set

    Code
      type_github_get_headers()
    Output
                                 Accept                      Content-Type 
       "application/vnd.github.v3+json" "application/json; charset=utf-8" 
                          Authorization                        User-Agent 
                       "token ci-token"                       "r-lib/pak" 

# builtin token is used if no other token is available

    Code
      type_github_get_headers()
    Output
                                 Accept                      Content-Type 
       "application/vnd.github.v3+json" "application/json; charset=utf-8" 
                          Authorization                        User-Agent 
                  "token builtin-token"                       "r-lib/pak" 

