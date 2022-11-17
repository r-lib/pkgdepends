# error

    Code
      res$error[[1]]
    Output
      <async_rejected/rlib_error_3_0/rlib_error/error>
      Error in `value[[3L]](cond)`:
      ! pkgdepends resolution error for foo::bar.
      Caused by error: 
      ! foobar

---

    Code
      res$error[[2]]
    Output
      <async_rejected/rlib_error_3_0/rlib_error/error>
      Error in `value[[3L]](cond)`:
      ! pkgdepends resolution error for foo::bar2.
      Caused by error: 
      ! foobar

