# resolve_remote

    Code
      res[, c("ref", "sources")]
    Output
      # A data frame: 10 x 2
         ref                         sources                                          
       * <chr>                       <chr>                                            
       1 crayon=github::r-lib/crayon /repos/r-lib/crayon/zipball/bdd9a1bcf062396790c3~
       2 crayon=r-lib/crayon         /repos/r-lib/crayon/zipball/bdd9a1bcf062396790c3~
       3 gaborcsardi/secret          /repos/gaborcsardi/secret/zipball/7f9fb08e26015e~
       4 gaborcsardi/secret@x        /repos/gaborcsardi/secret/zipball/7f9fb08e26015e~
       5 github::r-lib/crayon        /repos/r-lib/crayon/zipball/bdd9a1bcf062396790c3~
       6 r-lib/crayon                /repos/r-lib/crayon/zipball/bdd9a1bcf062396790c3~
       7 r-lib/crayon#79             /repos/r-lib/crayon/zipball/9d93692f8f7c1d6b2308~
       8 r-lib/crayon@b5221ab0246050 /repos/r-lib/crayon/zipball/b5221ab024605019800d~
       9 r-lib/pkgconfig#7           /repos/r-lib/pkgconfig/zipball/c9be9cde5e91ad771~
      10 wesm/feather/R              /repos/wesm/feather/zipball/ec40c1eae1ac83b86fc4~

---

    Code
      res$metadata
    Output
      [[1]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "crayon" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                           "crayon=github::r-lib/crayon" 
                                                               RemoteRef 
                                                                  "HEAD" 
                                                               RemoteSha 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
                                                              GithubRepo 
                                                                "crayon" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                               GithubRef 
                                                                  "HEAD" 
                                                              GithubSHA1 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
      
      [[2]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "crayon" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                                   "crayon=r-lib/crayon" 
                                                               RemoteRef 
                                                                  "HEAD" 
                                                               RemoteSha 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
                                                              GithubRepo 
                                                                "crayon" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                               GithubRef 
                                                                  "HEAD" 
                                                              GithubSHA1 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
      
      [[3]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "secret" 
                                                          RemoteUsername 
                                                           "gaborcsardi" 
                                                            RemotePkgRef 
                                                    "gaborcsardi/secret" 
                                                               RemoteRef 
                                                                  "HEAD" 
                                                               RemoteSha 
      "7f9fb08e26015e05529cd4d7fc2a7edbd88c783d456ff83a96dcc58ace1d3ea5" 
                                                              GithubRepo 
                                                                "secret" 
                                                          GithubUsername 
                                                           "gaborcsardi" 
                                                               GithubRef 
                                                                  "HEAD" 
                                                              GithubSHA1 
      "7f9fb08e26015e05529cd4d7fc2a7edbd88c783d456ff83a96dcc58ace1d3ea5" 
      
      [[4]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "secret" 
                                                          RemoteUsername 
                                                           "gaborcsardi" 
                                                            RemotePkgRef 
                                                  "gaborcsardi/secret@x" 
                                                               RemoteRef 
                                                                     "x" 
                                                               RemoteSha 
      "7f9fb08e26015e05529cd4d7fc2a7edbd88c783d456ff83a96dcc58ace1d3ea5" 
                                                              GithubRepo 
                                                                "secret" 
                                                          GithubUsername 
                                                           "gaborcsardi" 
                                                               GithubRef 
                                                                     "x" 
                                                              GithubSHA1 
      "7f9fb08e26015e05529cd4d7fc2a7edbd88c783d456ff83a96dcc58ace1d3ea5" 
      
      [[5]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "crayon" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                                  "github::r-lib/crayon" 
                                                               RemoteRef 
                                                                  "HEAD" 
                                                               RemoteSha 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
                                                              GithubRepo 
                                                                "crayon" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                               GithubRef 
                                                                  "HEAD" 
                                                              GithubSHA1 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
      
      [[6]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "crayon" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                                          "r-lib/crayon" 
                                                               RemoteRef 
                                                                  "HEAD" 
                                                               RemoteSha 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
                                                              GithubRepo 
                                                                "crayon" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                               GithubRef 
                                                                  "HEAD" 
                                                              GithubSHA1 
      "bdd9a1bcf062396790c341cf1dba563eb0277f2ca0a6d524bc3da98a9a6f2975" 
      
      [[7]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "crayon" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                                       "r-lib/crayon#79" 
                                                              RemotePull 
                                                                    "79" 
                                                               RemoteSha 
      "9d93692f8f7c1d6b2308d0c4aa83cdc2d99ec1fd0097cede1d9aa1301247cb01" 
                                                              GithubRepo 
                                                                "crayon" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                              GitHubPull 
                                                                    "79" 
                                                              GithubSHA1 
      "9d93692f8f7c1d6b2308d0c4aa83cdc2d99ec1fd0097cede1d9aa1301247cb01" 
      
      [[8]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                                "crayon" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                           "r-lib/crayon@b5221ab0246050" 
                                                               RemoteRef 
                                                        "b5221ab0246050" 
                                                               RemoteSha 
      "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2" 
                                                              GithubRepo 
                                                                "crayon" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                               GithubRef 
                                                        "b5221ab0246050" 
                                                              GithubSHA1 
      "b5221ab024605019800ddea474f7a0981a4d53f719f5af2b1af627b34e0760b2" 
      
      [[9]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                             "pkgconfig" 
                                                          RemoteUsername 
                                                                 "r-lib" 
                                                            RemotePkgRef 
                                                     "r-lib/pkgconfig#7" 
                                                              RemotePull 
                                                                     "7" 
                                                               RemoteSha 
      "c9be9cde5e91ad771d1b5150781e6e8d32a7be0e9ab227bdf45cb41ad513004c" 
                                                              GithubRepo 
                                                             "pkgconfig" 
                                                          GithubUsername 
                                                                 "r-lib" 
                                                              GitHubPull 
                                                                     "7" 
                                                              GithubSHA1 
      "c9be9cde5e91ad771d1b5150781e6e8d32a7be0e9ab227bdf45cb41ad513004c" 
      
      [[10]]
                                                              RemoteType 
                                                                "github" 
                                                              RemoteHost 
                                                      "127.0.0.1:<port>/" 
                                                              RemoteRepo 
                                                               "feather" 
                                                          RemoteUsername 
                                                                  "wesm" 
                                                            RemotePkgRef 
                                                        "wesm/feather/R" 
                                                               RemoteRef 
                                                                  "HEAD" 
                                                               RemoteSha 
      "ec40c1eae1ac83b86fc41bb2f5cd916152d19015649c3d209f2c08115dd993b1" 
                                                            RemoteSubdir 
                                                                     "R" 
                                                              GithubRepo 
                                                               "feather" 
                                                          GithubUsername 
                                                                  "wesm" 
                                                               GithubRef 
                                                                  "HEAD" 
                                                              GithubSHA1 
      "ec40c1eae1ac83b86fc41bb2f5cd916152d19015649c3d209f2c08115dd993b1" 
                                                            GithubSubdir 
                                                                     "R" 
      

