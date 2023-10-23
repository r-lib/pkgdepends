# initial state

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i No downloads are needed

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i No downloads are needed, 1 pkg (10 kB) is cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i No downloads are needed, 2 pkgs (20 kB) are cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i No downloads are needed, 1 pkg is cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 2 pkgs (30 kB)

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 2 pkgs (30 kB), 1 cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 2 pkgs (30 kB), 1 (20 kB) cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 1 pkg (20 kB) and 1 pkg with unknown size

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 1 pkg (10 kB) and 1 pkg with unknown size, 1 (30 kB) cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 1 pkg (10 kB) and 1 pkg with unknown size, 1 cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 2 pkgs with unknown sizes

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 2 pkgs with unknown sizes, 1 cached

---

    Code
      invisible(pkgplan__create_progress_bar(what))
    Message
      i Getting 2 pkgs with unknown sizes, 1 (30 kB) cached

# data updates

    Code
      bar <- pkgplan__create_progress_bar(what = what)
    Message
      i Getting 2 pkgs (30 kB), 1 (20 kB) cached
    Code
      pkgplan__update_progress_bar(bar, 1L, "data", list(current = 5000, total = 10000))
    Output
      [1] TRUE
    Code
      pkgplan__update_progress_bar(bar, 4L, "data", list(current = 20000, total = 20000))
    Output
      [1] TRUE
    Code
      pkgplan__done_progress_bar(bar)
    Output
      NULL
    Code
      bar$what
    Output
      # A tibble: 4 x 9
        type      filesize package cache_status   idx current  need status skip 
        <chr>        <dbl> <chr>   <chr>        <int>   <dbl> <dbl> <chr>  <lgl>
      1 cran         10000 foo     miss             1    5000 10000 data   FALSE
      2 installed    10000 foo2    <NA>             2       0 10000 skip   TRUE 
      3 cran         20000 bar     hit              3       0 20000 skip   TRUE 
      4 cran         20000 bar2    miss             4   20000 20000 data   FALSE

# all finish messages for updates

    Code
      do(1L)
    Message
      i Getting 2 pkgs (30 kB), 1 (20 kB) cached
      v Got foo 1.0.0 (source) (7 B)
    Output
      # A tibble: 4 x 9
        type      filesize package cache_status   idx current  need status skip 
        <chr>        <dbl> <chr>   <chr>        <int>   <dbl> <dbl> <chr>  <lgl>
      1 cran             7 foo     miss             1       7 10000 got    FALSE
      2 installed    10000 foo2    <NA>             2       0 10000 skip   TRUE 
      3 cran         20000 bar     hit              3       0 20000 skip   TRUE 
      4 cran         20000 bar2    miss             4       0 20000 todo   FALSE

---

    Code
      do(1L, make_tempfile = FALSE)
    Message
      i Getting 2 pkgs (30 kB), 1 (20 kB) cached
      v Got foo 1.0.0 (source)
    Output
      # A tibble: 4 x 9
        type      filesize package cache_status   idx current  need status skip 
        <chr>        <dbl> <chr>   <chr>        <int>   <dbl> <dbl> <chr>  <lgl>
      1 cran         10000 foo     miss             1   10000 10000 got    FALSE
      2 installed    10000 foo2    <NA>             2       0 10000 skip   TRUE 
      3 cran         20000 bar     hit              3       0 20000 skip   TRUE 
      4 cran         20000 bar2    miss             4       0 20000 todo   FALSE

---

    Code
      do(1L, event = "error")
    Message
      i Getting 2 pkgs (30 kB), 1 (20 kB) cached
      x Failed to download foo 1.0.0 (source)
    Output
      # A tibble: 4 x 9
        type      filesize package cache_status   idx current  need status skip 
        <chr>        <dbl> <chr>   <chr>        <int>   <int> <dbl> <chr>  <lgl>
      1 cran         10000 foo     miss             1       0     0 error  FALSE
      2 installed    10000 foo2    <NA>             2       0 10000 skip   TRUE 
      3 cran         20000 bar     hit              3       0 20000 skip   TRUE 
      4 cran         20000 bar2    miss             4       0 20000 todo   FALSE

# parts are calculated properly

    Code
      bar <- pkgplan__create_progress_bar(what)
    Message
      i Getting 2 pkgs (30 kB), 1 (20 kB) cached

