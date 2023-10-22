# install_extracted_binary, add_metadata

    Code
      dir(pkg_cache)
    Output
      [1] "foo"

---

    Code
      d <- desc::desc(file.path(lib, "foo"))
      d$get_field("Package")
    Output
      [1] "foo"
    Code
      d$get_field("Foo")
    Output
      [1] "bar"
    Code
      d$get_field("Foobar")
    Output
      [1] "baz"

---

    Code
      rds$DESCRIPTION[c("Package", "Foo", "Foobar")]
    Output
      Package     Foo  Foobar 
        "foo"   "bar"   "baz" 

---

    Code
      d <- desc::desc(file.path(lib, "foo"))
      d$get_field("Package")
    Output
      [1] "foo"
    Code
      d$get_field("Another")
    Output
      [1] "field"

---

    Code
      rds$DESCRIPTION[c("Package", "Another")]
    Output
      Package Another 
        "foo" "field" 

---

    Code
      dir(pkg_cache)
    Output
      [1] "foo"

---

    Code
      d <- desc::desc(file.path(lib, "foo"))
      d$get_field("Package")
    Output
      [1] "foo"
    Code
      d$get_field("Foo")
    Output
      [1] "bar2"
    Code
      d$get_field("Foobar")
    Output
      [1] "baz2"

---

    Code
      rds$DESCRIPTION[c("Package", "Foo", "Foobar")]
    Output
      Package     Foo  Foobar 
        "foo"  "bar2"  "baz2" 

# add_metadata error

    Code
      add_metadata(tmp, c(foo = "bar"))
    Condition
      Error:
      ! Could not find 'DESCRIPTION' file when installing package into '<tempdir>/<tempfile>'.
      i This is an internal error in pkgdepends, please report an issue at <https://github.com/r-lib/pkgdepends/issues>.

