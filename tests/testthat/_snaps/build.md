# vignettes can be turned on and off

    Code
      inst$solve()
      inst$download()
    Message
      i Getting 1 pkg with unknown size
      v Got pkgdependstest 1.0.0 (source)
    Code
      inst$install()
    Message
      i Packaging pkgdependstest 1.0.0
      v Packaged pkgdependstest 1.0.0
      i Building pkgdependstest 1.0.0
      v Built pkgdependstest 1.0.0
      v Installed pkgdependstest 1.0.0 (local)
      v Summary:   1 new

---

    Code
      inst2$solve()
      inst2$download()
    Message
      i Getting 1 pkg with unknown size
      v Got pkgdependstest 1.0.0 (source)
    Code
      inst2$install()
    Message
      i Packaging pkgdependstest 1.0.0
      v Packaged pkgdependstest 1.0.0
      i Building pkgdependstest 1.0.0
      v Built pkgdependstest 1.0.0
      v Installed pkgdependstest 1.0.0 (local)
      v Summary:   1 new

