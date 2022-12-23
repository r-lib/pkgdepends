# git_list_refs

    Code
      git_list_refs("https://github.com/gaborcsardi/pak-test.git")$refs
    Output
                      ref                                     hash
      1              HEAD 3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      2   refs/heads/main 3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      3 refs/heads/subdir cefdc0eebcd7f757efb9a80652fd8aaf1a87508e
      4      refs/tags/v1 cefdc0eebcd7f757efb9a80652fd8aaf1a87508e

---

    Code
      git_list_refs("https://github.com/gaborcsardi/pak-test.git", "refs/heads/")$refs
    Output
                      ref                                     hash
      1   refs/heads/main 3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      2 refs/heads/subdir cefdc0eebcd7f757efb9a80652fd8aaf1a87508e

# git_list_files

    Code
      git_list_files("https://github.com/gaborcsardi/pak-test.git",
        "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e")
    Output
      $ref
      [1] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
      
      $sha
      [1] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
      
      $commit
                                                          tree 
                    "aac34e05a0cb9852aa425757dc8480fa29c6c783" 
                                                        parent 
                    "cb8e35b36f8096ef604eff80c70259c64b34f125" 
                                                        author 
      "Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100" 
                                                     committer 
      "Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100" 
                                                       message 
                                                "Just bumping" 
      
      $tree
      [1] "aac34e05a0cb9852aa425757dc8480fa29c6c783"
      
      $files
                                             hash type   mode
      1  61102a589aeef025b8c11015d5de4fd6f91b0fdb tree  40000
      2  0784f430fc5830e1b26ceb5fa9a22551fbe9a400 tree  40000
      3  c3117c75084ec688a9bf82ce11789e0295f01ca3 tree  40000
      4  0acc01668acf00adc8c3921edea2526d70c67882 blob 100644
      5  23b72b52356f0a1ac89925f5431e44a0ab23bab6 tree  40000
      6  cddf411c750f674622bd53fe679a71698dadda4c blob 100644
      7  0d9009c1027504fd192ec1270c72581c7fc83825 blob 100644
      8  e69de29bb2d1d6434b8b29ae775ad8c2e48c5391 blob 100644
      9  5ba9d25318eacd311bb9f73cf83f859b0bfb7235 tree  40000
      10 75210921adb3ebdf805d8b01691e2d4b3915d096 tree  40000
      11 7a1a03c76050517831cc70db25e12f037fec8322 blob 100644
      12 61ffc7cb1f6d9afb3f7a57222b9a4a757a0239f3 blob 100644
      13 21806f7f52466fa018a94cd69b763c4a1cb10d1d blob 100644
      14 ce4f59a46da792e747fc9005a7958d3dcdca03cb blob 100644
      15 bfd18443a589f7c8e09ca9029ee4df9798c9ab00 blob 100644
      16 a87454ade264a8c407fc2f42b5e60a5a0307ef54 blob 100644
      17 9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2 tree  40000
      18 8ff50d0556b568d7e2ceaf541da110fff48d2668 blob 100644
      19 9c6833648770eedb4ec8a19002f345e816d62dae blob 100644
      20 1214f76975781d3b0c517f423b128037c6e450ed blob 100644
      21 d4ef766204f3087d32148d714df664ae66eece82 tree  40000
      22 fdd300286944e36a1c7f4b76d031f0bea4147d47 blob 100644
      23 dc24b6898cbc56eee682ca1c32735e72e819e4ab blob 100644
      24 a1e2d6741374d1f32ec138ee2020eae36b859e99 blob 100644
                                          path
      1                                .github
      2                        .github/actions
      3             .github/actions/parameters
      4  .github/actions/parameters/action.yml
      5                      .github/workflows
      6  .github/workflows/check-standard.yaml
      7                             .gitignore
      8                                    foo
      9                                 subdir
      10                         subdir/dotenv
      11           subdir/dotenv/.Rbuildignore
      12              subdir/dotenv/.gitignore
      13             subdir/dotenv/DESCRIPTION
      14                 subdir/dotenv/LICENSE
      15               subdir/dotenv/NAMESPACE
      16                 subdir/dotenv/NEWS.md
      17                       subdir/dotenv/R
      18      subdir/dotenv/R/dotenv-package.r
      19              subdir/dotenv/README.Rmd
      20               subdir/dotenv/README.md
      21                     subdir/dotenv/man
      22   subdir/dotenv/man/dotenv-package.Rd
      23     subdir/dotenv/man/load_dot_env.Rd
      24                                wipe.R
      
    Code
      git_list_files("https://github.com/gaborcsardi/pak-test.git", "refs/tags/v1")
    Output
      $ref
      [1] "refs/tags/v1"
      
      $sha
      [1] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
      
      $commit
                                                          tree 
                    "aac34e05a0cb9852aa425757dc8480fa29c6c783" 
                                                        parent 
                    "cb8e35b36f8096ef604eff80c70259c64b34f125" 
                                                        author 
      "Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100" 
                                                     committer 
      "Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100" 
                                                       message 
                                                "Just bumping" 
      
      $tree
      [1] "aac34e05a0cb9852aa425757dc8480fa29c6c783"
      
      $files
                                             hash type   mode
      1  61102a589aeef025b8c11015d5de4fd6f91b0fdb tree  40000
      2  0784f430fc5830e1b26ceb5fa9a22551fbe9a400 tree  40000
      3  c3117c75084ec688a9bf82ce11789e0295f01ca3 tree  40000
      4  0acc01668acf00adc8c3921edea2526d70c67882 blob 100644
      5  23b72b52356f0a1ac89925f5431e44a0ab23bab6 tree  40000
      6  cddf411c750f674622bd53fe679a71698dadda4c blob 100644
      7  0d9009c1027504fd192ec1270c72581c7fc83825 blob 100644
      8  e69de29bb2d1d6434b8b29ae775ad8c2e48c5391 blob 100644
      9  5ba9d25318eacd311bb9f73cf83f859b0bfb7235 tree  40000
      10 75210921adb3ebdf805d8b01691e2d4b3915d096 tree  40000
      11 7a1a03c76050517831cc70db25e12f037fec8322 blob 100644
      12 61ffc7cb1f6d9afb3f7a57222b9a4a757a0239f3 blob 100644
      13 21806f7f52466fa018a94cd69b763c4a1cb10d1d blob 100644
      14 ce4f59a46da792e747fc9005a7958d3dcdca03cb blob 100644
      15 bfd18443a589f7c8e09ca9029ee4df9798c9ab00 blob 100644
      16 a87454ade264a8c407fc2f42b5e60a5a0307ef54 blob 100644
      17 9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2 tree  40000
      18 8ff50d0556b568d7e2ceaf541da110fff48d2668 blob 100644
      19 9c6833648770eedb4ec8a19002f345e816d62dae blob 100644
      20 1214f76975781d3b0c517f423b128037c6e450ed blob 100644
      21 d4ef766204f3087d32148d714df664ae66eece82 tree  40000
      22 fdd300286944e36a1c7f4b76d031f0bea4147d47 blob 100644
      23 dc24b6898cbc56eee682ca1c32735e72e819e4ab blob 100644
      24 a1e2d6741374d1f32ec138ee2020eae36b859e99 blob 100644
                                          path
      1                                .github
      2                        .github/actions
      3             .github/actions/parameters
      4  .github/actions/parameters/action.yml
      5                      .github/workflows
      6  .github/workflows/check-standard.yaml
      7                             .gitignore
      8                                    foo
      9                                 subdir
      10                         subdir/dotenv
      11           subdir/dotenv/.Rbuildignore
      12              subdir/dotenv/.gitignore
      13             subdir/dotenv/DESCRIPTION
      14                 subdir/dotenv/LICENSE
      15               subdir/dotenv/NAMESPACE
      16                 subdir/dotenv/NEWS.md
      17                       subdir/dotenv/R
      18      subdir/dotenv/R/dotenv-package.r
      19              subdir/dotenv/README.Rmd
      20               subdir/dotenv/README.md
      21                     subdir/dotenv/man
      22   subdir/dotenv/man/dotenv-package.Rd
      23     subdir/dotenv/man/load_dot_env.Rd
      24                                wipe.R
      

# git_download_file

    Code
      out <- git_download_file("https://github.com/gaborcsardi/pak-test.git",
        "a1e2d6741374d1f32ec138ee2020eae36b859e99", tmp)
      out
    Output
      $type
      [1] "blob"
      
      $raw
        [1] 2e 64 79 6e 4c 69 62 73 28 29 0a 70 6b 67 6c 6f 61 64 3a 3a 6c 6f 61 64 5f
       [26] 61 6c 6c 28 29 0a 2e 64 79 6e 4c 69 62 73 28 29 0a 67 63 28 29 0a 2e 64 79
       [51] 6e 4c 69 62 73 28 29 0a 70 6b 67 6c 6f 61 64 3a 3a 6c 6f 61 64 5f 61 6c 6c
       [76] 28 29 0a 2e 64 79 6e 4c 69 62 73 28 29 0a 67 63 28 29 0a 2e 64 79 6e 4c 69
      [101] 62 73 28 29 0a
      
      $size
      [1] 105
      
      $packed_size
      [1] 44
      
      $hash
      [1] "a1e2d6741374d1f32ec138ee2020eae36b859e99"
      
    Code
      readLines(tmp)
    Output
      [1] ".dynLibs()"          "pkgload::load_all()" ".dynLibs()"         
      [4] "gc()"                ".dynLibs()"          "pkgload::load_all()"
      [7] ".dynLibs()"          "gc()"                ".dynLibs()"         

# git_unpack

    Code
      git_unpack(path)
    Output
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e$type
      [1] "commit"
      
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e$raw
        [1] 74 72 65 65 20 61 61 63 33 34 65 30 35 61 30 63 62 39 38 35 32 61 61 34 32
       [26] 35 37 35 37 64 63 38 34 38 30 66 61 32 39 63 36 63 37 38 33 0a 70 61 72 65
       [51] 6e 74 20 63 62 38 65 33 35 62 33 36 66 38 30 39 36 65 66 36 30 34 65 66 66
       [76] 38 30 63 37 30 32 35 39 63 36 34 62 33 34 66 31 32 35 0a 61 75 74 68 6f 72
      [101] 20 47 c3 a1 62 6f 72 20 43 73 c3 a1 72 64 69 20 3c 63 73 61 72 64 69 2e 67
      [126] 61 62 6f 72 40 67 6d 61 69 6c 2e 63 6f 6d 3e 20 31 36 33 39 31 36 35 34 39
      [151] 36 20 2b 30 31 30 30 0a 63 6f 6d 6d 69 74 74 65 72 20 47 c3 a1 62 6f 72 20
      [176] 43 73 c3 a1 72 64 69 20 3c 63 73 61 72 64 69 2e 67 61 62 6f 72 40 67 6d 61
      [201] 69 6c 2e 63 6f 6d 3e 20 31 36 33 39 31 36 35 34 39 36 20 2b 30 31 30 30 0a
      [226] 0a 4a 75 73 74 20 62 75 6d 70 69 6e 67 0a
      
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e$size
      [1] 239
      
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e$packed_size
      [1] 161
      
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e$object
      [1] "tree aac34e05a0cb9852aa425757dc8480fa29c6c783\nparent cb8e35b36f8096ef604eff80c70259c64b34f125\nauthor Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100\ncommitter Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100\n\nJust bumping\n"
      
      $cefdc0eebcd7f757efb9a80652fd8aaf1a87508e$hash
      [1] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
      
      
      $aac34e05a0cb9852aa425757dc8480fa29c6c783
      $aac34e05a0cb9852aa425757dc8480fa29c6c783$type
      [1] "tree"
      
      $aac34e05a0cb9852aa425757dc8480fa29c6c783$raw
        [1] 34 30 30 30 30 20 2e 67 69 74 68 75 62 00 61 10 2a 58 9a ee f0 25 b8 c1 10
       [26] 15 d5 de 4f d6 f9 1b 0f db 31 30 30 36 34 34 20 2e 67 69 74 69 67 6e 6f 72
       [51] 65 00 0d 90 09 c1 02 75 04 fd 19 2e c1 27 0c 72 58 1c 7f c8 38 25 31 30 30
       [76] 36 34 34 20 66 6f 6f 00 e6 9d e2 9b b2 d1 d6 43 4b 8b 29 ae 77 5a d8 c2 e4
      [101] 8c 53 91 34 30 30 30 30 20 73 75 62 64 69 72 00 5b a9 d2 53 18 ea cd 31 1b
      [126] b9 f7 3c f8 3f 85 9b 0b fb 72 35 31 30 30 36 34 34 20 77 69 70 65 2e 52 00
      [151] a1 e2 d6 74 13 74 d1 f3 2e c1 38 ee 20 20 ea e3 6b 85 9e 99
      
      $aac34e05a0cb9852aa425757dc8480fa29c6c783$size
      [1] 170
      
      $aac34e05a0cb9852aa425757dc8480fa29c6c783$packed_size
      [1] 166
      
      $aac34e05a0cb9852aa425757dc8480fa29c6c783$object
        type   mode       path                                     hash
      1 tree  40000    .github 61102a589aeef025b8c11015d5de4fd6f91b0fdb
      2 blob 100644 .gitignore 0d9009c1027504fd192ec1270c72581c7fc83825
      3 blob 100644        foo e69de29bb2d1d6434b8b29ae775ad8c2e48c5391
      4 tree  40000     subdir 5ba9d25318eacd311bb9f73cf83f859b0bfb7235
      5 blob 100644     wipe.R a1e2d6741374d1f32ec138ee2020eae36b859e99
      
      $aac34e05a0cb9852aa425757dc8480fa29c6c783$hash
      [1] "aac34e05a0cb9852aa425757dc8480fa29c6c783"
      
      
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`$type
      [1] "tree"
      
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`$raw
       [1] 34 30 30 30 30 20 61 63 74 69 6f 6e 73 00 07 84 f4 30 fc 58 30 e1 b2 6c eb
      [26] 5f a9 a2 25 51 fb e9 a4 00 34 30 30 30 30 20 77 6f 72 6b 66 6c 6f 77 73 00
      [51] 23 b7 2b 52 35 6f 0a 1a c8 99 25 f5 43 1e 44 a0 ab 23 ba b6
      
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`$size
      [1] 70
      
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`$packed_size
      [1] 75
      
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`$object
        type  mode      path                                     hash
      1 tree 40000   actions 0784f430fc5830e1b26ceb5fa9a22551fbe9a400
      2 tree 40000 workflows 23b72b52356f0a1ac89925f5431e44a0ab23bab6
      
      $`61102a589aeef025b8c11015d5de4fd6f91b0fdb`$hash
      [1] "61102a589aeef025b8c11015d5de4fd6f91b0fdb"
      
      
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`$type
      [1] "tree"
      
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`$raw
       [1] 34 30 30 30 30 20 70 61 72 61 6d 65 74 65 72 73 00 c3 11 7c 75 08 4e c6 88
      [26] a9 bf 82 ce 11 78 9e 02 95 f0 1c a3
      
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`$size
      [1] 37
      
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`$packed_size
      [1] 44
      
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`$object
        type  mode       path                                     hash
      1 tree 40000 parameters c3117c75084ec688a9bf82ce11789e0295f01ca3
      
      $`0784f430fc5830e1b26ceb5fa9a22551fbe9a400`$hash
      [1] "0784f430fc5830e1b26ceb5fa9a22551fbe9a400"
      
      
      $c3117c75084ec688a9bf82ce11789e0295f01ca3
      $c3117c75084ec688a9bf82ce11789e0295f01ca3$type
      [1] "tree"
      
      $c3117c75084ec688a9bf82ce11789e0295f01ca3$raw
       [1] 31 30 30 36 34 34 20 61 63 74 69 6f 6e 2e 79 6d 6c 00 0a cc 01 66 8a cf 00
      [26] ad c8 c3 92 1e de a2 52 6d 70 c6 78 82
      
      $c3117c75084ec688a9bf82ce11789e0295f01ca3$size
      [1] 38
      
      $c3117c75084ec688a9bf82ce11789e0295f01ca3$packed_size
      [1] 47
      
      $c3117c75084ec688a9bf82ce11789e0295f01ca3$object
        type   mode       path                                     hash
      1 blob 100644 action.yml 0acc01668acf00adc8c3921edea2526d70c67882
      
      $c3117c75084ec688a9bf82ce11789e0295f01ca3$hash
      [1] "c3117c75084ec688a9bf82ce11789e0295f01ca3"
      
      
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`$type
      [1] "tree"
      
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`$raw
       [1] 31 30 30 36 34 34 20 63 68 65 63 6b 2d 73 74 61 6e 64 61 72 64 2e 79 61 6d
      [26] 6c 00 cd df 41 1c 75 0f 67 46 22 bd 53 fe 67 9a 71 69 8d ad da 4c
      
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`$size
      [1] 47
      
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`$packed_size
      [1] 56
      
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`$object
        type   mode                path                                     hash
      1 blob 100644 check-standard.yaml cddf411c750f674622bd53fe679a71698dadda4c
      
      $`23b72b52356f0a1ac89925f5431e44a0ab23bab6`$hash
      [1] "23b72b52356f0a1ac89925f5431e44a0ab23bab6"
      
      
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`$type
      [1] "tree"
      
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`$raw
       [1] 34 30 30 30 30 20 64 6f 74 65 6e 76 00 75 21 09 21 ad b3 eb df 80 5d 8b 01
      [26] 69 1e 2d 4b 39 15 d0 96
      
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`$size
      [1] 33
      
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`$packed_size
      [1] 40
      
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`$object
        type  mode   path                                     hash
      1 tree 40000 dotenv 75210921adb3ebdf805d8b01691e2d4b3915d096
      
      $`5ba9d25318eacd311bb9f73cf83f859b0bfb7235`$hash
      [1] "5ba9d25318eacd311bb9f73cf83f859b0bfb7235"
      
      
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`$type
      [1] "tree"
      
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`$raw
        [1] 31 30 30 36 34 34 20 2e 52 62 75 69 6c 64 69 67 6e 6f 72 65 00 7a 1a 03 c7
       [26] 60 50 51 78 31 cc 70 db 25 e1 2f 03 7f ec 83 22 31 30 30 36 34 34 20 2e 67
       [51] 69 74 69 67 6e 6f 72 65 00 61 ff c7 cb 1f 6d 9a fb 3f 7a 57 22 2b 9a 4a 75
       [76] 7a 02 39 f3 31 30 30 36 34 34 20 44 45 53 43 52 49 50 54 49 4f 4e 00 21 80
      [101] 6f 7f 52 46 6f a0 18 a9 4c d6 9b 76 3c 4a 1c b1 0d 1d 31 30 30 36 34 34 20
      [126] 4c 49 43 45 4e 53 45 00 ce 4f 59 a4 6d a7 92 e7 47 fc 90 05 a7 95 8d 3d cd
      [151] ca 03 cb 31 30 30 36 34 34 20 4e 41 4d 45 53 50 41 43 45 00 bf d1 84 43 a5
      [176] 89 f7 c8 e0 9c a9 02 9e e4 df 97 98 c9 ab 00 31 30 30 36 34 34 20 4e 45 57
      [201] 53 2e 6d 64 00 a8 74 54 ad e2 64 a8 c4 07 fc 2f 42 b5 e6 0a 5a 03 07 ef 54
      [226] 34 30 30 30 30 20 52 00 9a 09 c0 54 0f 29 a3 f0 e0 cd 0b f9 ac d7 30 3e 1f
      [251] a9 bf f2 31 30 30 36 34 34 20 52 45 41 44 4d 45 2e 52 6d 64 00 9c 68 33 64
      [276] 87 70 ee db 4e c8 a1 90 02 f3 45 e8 16 d6 2d ae 31 30 30 36 34 34 20 52 45
      [301] 41 44 4d 45 2e 6d 64 00 12 14 f7 69 75 78 1d 3b 0c 51 7f 42 3b 12 80 37 c6
      [326] e4 50 ed 34 30 30 30 30 20 6d 61 6e 00 d4 ef 76 62 04 f3 08 7d 32 14 8d 71
      [351] 4d f6 64 ae 66 ee ce 82
      
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`$size
      [1] 358
      
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`$packed_size
      [1] 321
      
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`$object
         type   mode          path                                     hash
      1  blob 100644 .Rbuildignore 7a1a03c76050517831cc70db25e12f037fec8322
      2  blob 100644    .gitignore 61ffc7cb1f6d9afb3f7a57222b9a4a757a0239f3
      3  blob 100644   DESCRIPTION 21806f7f52466fa018a94cd69b763c4a1cb10d1d
      4  blob 100644       LICENSE ce4f59a46da792e747fc9005a7958d3dcdca03cb
      5  blob 100644     NAMESPACE bfd18443a589f7c8e09ca9029ee4df9798c9ab00
      6  blob 100644       NEWS.md a87454ade264a8c407fc2f42b5e60a5a0307ef54
      7  tree  40000             R 9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2
      8  blob 100644    README.Rmd 9c6833648770eedb4ec8a19002f345e816d62dae
      9  blob 100644     README.md 1214f76975781d3b0c517f423b128037c6e450ed
      10 tree  40000           man d4ef766204f3087d32148d714df664ae66eece82
      
      $`75210921adb3ebdf805d8b01691e2d4b3915d096`$hash
      [1] "75210921adb3ebdf805d8b01691e2d4b3915d096"
      
      
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`$type
      [1] "tree"
      
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`$raw
       [1] 31 30 30 36 34 34 20 64 6f 74 65 6e 76 2d 70 61 63 6b 61 67 65 2e 72 00 8f
      [26] f5 0d 05 56 b5 68 d7 e2 ce af 54 1d a1 10 ff f4 8d 26 68
      
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`$size
      [1] 44
      
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`$packed_size
      [1] 53
      
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`$object
        type   mode             path                                     hash
      1 blob 100644 dotenv-package.r 8ff50d0556b568d7e2ceaf541da110fff48d2668
      
      $`9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2`$hash
      [1] "9a09c0540f29a3f0e0cd0bf9acd7303e1fa9bff2"
      
      
      $d4ef766204f3087d32148d714df664ae66eece82
      $d4ef766204f3087d32148d714df664ae66eece82$type
      [1] "tree"
      
      $d4ef766204f3087d32148d714df664ae66eece82$raw
       [1] 31 30 30 36 34 34 20 64 6f 74 65 6e 76 2d 70 61 63 6b 61 67 65 2e 52 64 00
      [26] fd d3 00 28 69 44 e3 6a 1c 7f 4b 76 d0 31 f0 be a4 14 7d 47 31 30 30 36 34
      [51] 34 20 6c 6f 61 64 5f 64 6f 74 5f 65 6e 76 2e 52 64 00 dc 24 b6 89 8c bc 56
      [76] ee e6 82 ca 1c 32 73 5e 72 e8 19 e4 ab
      
      $d4ef766204f3087d32148d714df664ae66eece82$size
      [1] 88
      
      $d4ef766204f3087d32148d714df664ae66eece82$packed_size
      [1] 90
      
      $d4ef766204f3087d32148d714df664ae66eece82$object
        type   mode              path                                     hash
      1 blob 100644 dotenv-package.Rd fdd300286944e36a1c7f4b76d031f0bea4147d47
      2 blob 100644   load_dot_env.Rd dc24b6898cbc56eee682ca1c32735e72e819e4ab
      
      $d4ef766204f3087d32148d714df664ae66eece82$hash
      [1] "d4ef766204f3087d32148d714df664ae66eece82"
      
      

