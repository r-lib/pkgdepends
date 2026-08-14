test_that("platform_is_ok", {
  ok <- list(
    list("*", "source", NULL),
    list("*", c("x86_64-w64-ming32", "source"), NULL),
    list("*", "source", "prefer-x64"),
    list("*", "source", "both"),
    list("source", c("x86_64-w64-mingw32", "source"), NULL),
    list("source", "source", NULL),
    list("source", "source", "prefer-x64"),
    list("source", "source", "both"),
    list(
      "i386+x86_64-w64-mingw32",
      c("x86_64-w64-mingw32", "source"),
      "prefer-x64"
    ),
    list("i386+x86_64-w64-mingw32", c("i386-w64-mingw32", "source"), NULL),
    list("i386+x86_64-w64-mingw32", "x86_64-w64-mingw32", "prefer-x64"),
    list("i386+x86_64-w64-mingw32", "i386-w64-mingw32", NULL),
    list("x86_64-w64-mingw32", "x86_64-w64-mingw32", "prefer-x64"),
    list(
      "aarch64-unknown-linux-gnu-manylinux-2.28",
      c("aarch64-unknown-linux-gnu-ubuntu-24.04", "source"),
      NULL
    ),
    list(
      "x86_64-pc-linux-musl-manylinux-2.28",
      c("x86_64-pc-linux-musl-alpine-3.21.0", "source"),
      NULL
    )
  )
  for (c in ok) {
    expect_true(platform_is_ok(c[[1]], c[[2]], c[[3]]), info = c)
  }

  bad <- list(
    list("i386+x86_64-w64-mingw32", "source", "prefer-x64"),
    list("x86_64-w64-mingw32", "source", "prefer-x64"),
    list("i386-w64-mingw32", "source", "prefer-x64"),
    list("x86_64-apple-darwin17.0", "source", NULL),
    list("x86_64-w64-mingw32", "x86_64-w64-mingw32", "both"),
    list("i386-w64-mingw32", "i386-w64-mingw32", "both"),
    list(
      "x86_64-pc-linux-gnu-manylinux-2.28",
      c("aarch64-unknown-linux-gnu-ubuntu-24.04", "source"),
      NULL
    ),
    list(
      "aarch64-unknown-linux-gnu-manylinux-2.28",
      "aarch64-apple-darwin20",
      NULL
    )
  )
  for (c in bad) {
    expect_false(platform_is_ok(c[[1]], c[[2]], c[[3]]), info = c)
  }
})
