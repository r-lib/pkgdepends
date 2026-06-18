# resolve

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "empty"
    Code
      res$version
    Output
      [1] "1.0.0"
    Code
      res$metadata[[1]]
    Output
                                          RemoteType 
                                            "gitlab" 
                                           RemoteUrl 
          "http://127.0.0.1:<port>/repo/pak-test.git" 
                                        RemotePkgRef 
      "gitlab::http://127.0.0.1:<port>/repo/pak-test" 
                                           RemoteRef 
                                              "HEAD" 
                                           RemoteSha 
          "3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b" 
                                          RemoteHost 
                                   "127.0.0.1:<port>" 
                                          RemoteRepo 
                                          "pak-test" 
                                      RemoteUsername 
                                              "repo" 

---

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "empty"
    Code
      res$version
    Output
      [1] "1.0.0"
    Code
      res$metadata[[1]]
    Output
                                                       RemoteType 
                                                         "gitlab" 
                                                        RemoteUrl 
                       "http://127.0.0.1:<port>/repo/pak-test.git" 
                                                     RemotePkgRef 
      "gitlab::http://127.0.0.1:<port>/repo/pak-test@build-ignore" 
                                                        RemoteRef 
                                                   "build-ignore" 
                                                        RemoteSha 
                       "a9ffc55f59e0567ecdc67fb3f0333eca49be8d03" 
                                                       RemoteHost 
                                                "127.0.0.1:<port>" 
                                                       RemoteRepo 
                                                       "pak-test" 
                                                   RemoteUsername 
                                                           "repo" 

---

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "dotenv"
    Code
      res$version
    Output
      [1] "1.0.3.9000"
    Code
      res$metadata[[1]]
    Output
                                                             RemoteType 
                                                               "gitlab" 
                                                              RemoteUrl 
                             "http://127.0.0.1:<port>/repo/pak-test.git" 
                                                           RemotePkgRef 
      "gitlab::http://127.0.0.1:<port>/repo/pak-test/-/subdir/dotenv@v1" 
                                                              RemoteRef 
                                                                   "v1" 
                                                              RemoteSha 
                             "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e" 
                                                             RemoteHost 
                                                      "127.0.0.1:<port>" 
                                                             RemoteRepo 
                                                             "pak-test" 
                                                         RemoteUsername 
                                                                 "repo" 
                                                           RemoteSubdir 
                                                        "subdir/dotenv" 

---

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "dotenv"
    Code
      res$version
    Output
      [1] "1.0.3.9000"
    Code
      res$metadata[[1]]
    Output
                                                                 RemoteType 
                                                                   "gitlab" 
                                                                  RemoteUrl 
                                 "http://127.0.0.1:<port>/repo/pak-test.git" 
                                                               RemotePkgRef 
      "gitlab::http://127.0.0.1:<port>/repo/pak-test/-/subdir/dotenv@subdir" 
                                                                  RemoteRef 
                                                                   "subdir" 
                                                                  RemoteSha 
                                 "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e" 
                                                                 RemoteHost 
                                                          "127.0.0.1:<port>" 
                                                                 RemoteRepo 
                                                                 "pak-test" 
                                                             RemoteUsername 
                                                                     "repo" 
                                                               RemoteSubdir 
                                                            "subdir/dotenv" 

# download

    Code
      dir(file.path(dl$fulltarget_tree, "dotenv", "subdir", "dotenv"))
    Output
      [1] "DESCRIPTION" "LICENSE"     "NAMESPACE"   "NEWS.md"     "R"          
      [6] "README.Rmd"  "README.md"   "man"        

