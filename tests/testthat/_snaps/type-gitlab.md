# resolve

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "cli"
    Code
      res$version
    Output
      [1] "3.5.0.9000"
    Code
      res$metadata[[1]]
    Output
                                      RemoteType 
                                        "gitlab" 
                                       RemoteUrl 
        "https://gitlab.com/gaborcsardi/cli.git" 
                                    RemotePkgRef 
                       "gitlab::gaborcsardi/cli" 
                                       RemoteRef 
                                          "HEAD" 
                                       RemoteSha 
      "bc503509cddb65d0007aeb301936e27de76bc7d7" 
                                      RemoteHost 
                                    "gitlab.com" 
                                      RemoteRepo 
                                           "cli" 
                                  RemoteUsername 
                                   "gaborcsardi" 

---

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "filelock"
    Code
      res$version
    Output
      [1] "1.0.2"
    Code
      res$metadata[[1]]
    Output
                                      RemoteType 
                                        "gitlab" 
                                       RemoteUrl 
         "https://gitlab.com/r-hub/filelock.git" 
                                    RemotePkgRef 
             "gitlab::r-hub/filelock@cran-1-0-2" 
                                       RemoteRef 
                                    "cran-1-0-2" 
                                       RemoteSha 
      "dfc615b14dfc030df3f44bd377e752728f76df3d" 
                                      RemoteHost 
                                    "gitlab.com" 
                                      RemoteRepo 
                                      "filelock" 
                                  RemoteUsername 
                                         "r-hub" 

---

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "filelock"
    Code
      res$version
    Output
      [1] "1.0.2"
    Code
      res$metadata[[1]]
    Output
                                      RemoteType 
                                        "gitlab" 
                                       RemoteUrl 
         "https://gitlab.com/r-hub/filelock.git" 
                                    RemotePkgRef 
                 "gitlab::r-hub/filelock@v1.0.2" 
                                       RemoteRef 
                                        "v1.0.2" 
                                       RemoteSha 
      "9fdba75a62facaa3e818902f58891166e45eabe9" 
                                      RemoteHost 
                                    "gitlab.com" 
                                      RemoteRepo 
                                      "filelock" 
                                  RemoteUsername 
                                         "r-hub" 

---

    Code
      res$error
    Output
      [[1]]
      list()
      
    Code
      res$package
    Output
      [1] "feather"
    Code
      res$version
    Output
      [1] "0.3.5.9000"
    Code
      res$metadata[[1]]
    Output
                                        RemoteType 
                                          "gitlab" 
                                         RemoteUrl 
      "https://gitlab.com/gaborcsardi/feather.git" 
                                      RemotePkgRef 
                   "gitlab::gaborcsardi/feather/R" 
                                         RemoteRef 
                                            "HEAD" 
                                         RemoteSha 
        "3635466429b5ef7910da606267d0a339cf1c9821" 
                                        RemoteHost 
                                      "gitlab.com" 
                                        RemoteRepo 
                                         "feather" 
                                    RemoteUsername 
                                     "gaborcsardi" 
                                      RemoteSubdir 
                                               "R" 

# download

    Code
      dir(file.path(dl$fulltarget_tree, "feather", "R"))
    Output
       [1] "DESCRIPTION"      "NAMESPACE"        "NEWS.md"          "R"               
       [5] "README.md"        "cran-comments.md" "feather.Rproj"    "inst"            
       [9] "man"              "revdep"           "tests"           

