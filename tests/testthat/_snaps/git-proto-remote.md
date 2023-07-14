# git_list_refs

    Code
      git_list_refs("https://github.com/gaborcsardi/pak-test.git")$refs
    Output
      # A data frame: 4 x 2
        ref               hash                                    
        <chr>             <chr>                                   
      1 HEAD              3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      2 refs/heads/main   3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      3 refs/heads/subdir cefdc0eebcd7f757efb9a80652fd8aaf1a87508e
      4 refs/tags/v1      cefdc0eebcd7f757efb9a80652fd8aaf1a87508e

---

    Code
      git_list_refs_v2("https://github.com/gaborcsardi/pak-test.git", "refs/heads/")$
        refs
    Output
      # A data frame: 2 x 2
        ref               hash                                    
        <chr>             <chr>                                   
      1 refs/heads/main   3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
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
      # A data frame: 24 x 4
         hash                                     type  mode   path                   
         <chr>                                    <chr> <chr>  <chr>                  
       1 61102a589aeef025b8c11015d5de4fd6f91b0fdb tree  40000  .github                
       2 0784f430fc5830e1b26ceb5fa9a22551fbe9a400 tree  40000  .github/actions        
       3 c3117c75084ec688a9bf82ce11789e0295f01ca3 tree  40000  .github/actions/parame~
       4 0acc01668acf00adc8c3921edea2526d70c67882 blob  100644 .github/actions/parame~
       5 23b72b52356f0a1ac89925f5431e44a0ab23bab6 tree  40000  .github/workflows      
       6 cddf411c750f674622bd53fe679a71698dadda4c blob  100644 .github/workflows/chec~
       7 0d9009c1027504fd192ec1270c72581c7fc83825 blob  100644 .gitignore             
       8 e69de29bb2d1d6434b8b29ae775ad8c2e48c5391 blob  100644 foo                    
       9 5ba9d25318eacd311bb9f73cf83f859b0bfb7235 tree  40000  subdir                 
      10 75210921adb3ebdf805d8b01691e2d4b3915d096 tree  40000  subdir/dotenv          
      # i 14 more rows
      
    Code
      git_list_files("https://github.com/gaborcsardi/pak-test.git", "refs/tags/v1")
    Output
      $ref
      [1] "refs/tags/v1"
      
      $sha
      [1] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
      attr(,"protocol")
      [1] 2
      
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
      # A data frame: 24 x 4
         hash                                     type  mode   path                   
         <chr>                                    <chr> <chr>  <chr>                  
       1 61102a589aeef025b8c11015d5de4fd6f91b0fdb tree  40000  .github                
       2 0784f430fc5830e1b26ceb5fa9a22551fbe9a400 tree  40000  .github/actions        
       3 c3117c75084ec688a9bf82ce11789e0295f01ca3 tree  40000  .github/actions/parame~
       4 0acc01668acf00adc8c3921edea2526d70c67882 blob  100644 .github/actions/parame~
       5 23b72b52356f0a1ac89925f5431e44a0ab23bab6 tree  40000  .github/workflows      
       6 cddf411c750f674622bd53fe679a71698dadda4c blob  100644 .github/workflows/chec~
       7 0d9009c1027504fd192ec1270c72581c7fc83825 blob  100644 .gitignore             
       8 e69de29bb2d1d6434b8b29ae775ad8c2e48c5391 blob  100644 foo                    
       9 5ba9d25318eacd311bb9f73cf83f859b0bfb7235 tree  40000  subdir                 
      10 75210921adb3ebdf805d8b01691e2d4b3915d096 tree  40000  subdir/dotenv          
      # i 14 more rows
      

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

# git_fetch

    Code
      cat(pack[[1]]$object)
    Output
      tree aac34e05a0cb9852aa425757dc8480fa29c6c783
      parent cb8e35b36f8096ef604eff80c70259c64b34f125
      author Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100
      committer Gábor Csárdi <csardi.gabor@gmail.com> 1639165496 +0100
      
      Just bumping

# git_list_refs_v1

    Code
      git_list_refs_v1("https://github.com/gaborcsardi/pak-test.git")$refs
    Output
      # A data frame: 4 x 2
        ref               hash                                    
        <chr>             <chr>                                   
      1 HEAD              3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      2 refs/heads/main   3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b
      3 refs/heads/subdir cefdc0eebcd7f757efb9a80652fd8aaf1a87508e
      4 refs/tags/v1      cefdc0eebcd7f757efb9a80652fd8aaf1a87508e

# git_list_refs_v1_process_1

    Code
      git_list_refs_v1_process_1(resp, "https://github.com/gaborcsardi/pak-test.git",
        "refs/tags/v1")$refs
    Output
      # A data frame: 1 x 2
        ref          hash                                    
      * <chr>        <chr>                                   
      1 refs/tags/v1 cefdc0eebcd7f757efb9a80652fd8aaf1a87508e

# async_git_resolve_ref

    Code
      sy(async_git_resolve_ref("https://github.com/gaborcsardi/pak-test.git", "main"))
    Output
      [1] "3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b"
      attr(,"protocol")
      [1] 2

---

    Code
      sy(async_git_resolve_ref("https://github.com/gaborcsardi/pak-test.git", "v1"))
    Output
      [1] "cefdc0eebcd7f757efb9a80652fd8aaf1a87508e"
      attr(,"protocol")
      [1] 2

---

    Code
      sy(async_git_resolve_ref("https://github.com/gaborcsardi/pak-test.git",
        "3f3b0b4ee8a0ff"))
    Output
      [1] "3f3b0b4ee8a0ff4563073924e5fe069da67a6d8b"
      attr(,"protocol")
      [1] 2

---

    Code
      sy(async_git_resolve_ref("https://github.com/gaborcsardi/pak-test.git",
        "badcafe"))
    Error <async_rejected>
      ! Unknown git ref: "badcafe".

---

    Code
      sy(async_git_resolve_ref("https://github.com/gaborcsardi/pak-test.git",
        "badcafe"))
    Error <async_rejected>
      ! Found multiple git refs with prefix "badcafe", it is ambiguous.
      i Matching git refs: "badcafe1" and "badcafe2".
      i Specify a longer prefix to choose a single git ref.

# git_download_repo

    Code
      dir(tmp, recursive = TRUE)
    Output
       [1] "v1/foo"                                
       [2] "v1/subdir/dotenv/DESCRIPTION"          
       [3] "v1/subdir/dotenv/LICENSE"              
       [4] "v1/subdir/dotenv/NAMESPACE"            
       [5] "v1/subdir/dotenv/NEWS.md"              
       [6] "v1/subdir/dotenv/R/dotenv-package.r"   
       [7] "v1/subdir/dotenv/README.Rmd"           
       [8] "v1/subdir/dotenv/README.md"            
       [9] "v1/subdir/dotenv/man/dotenv-package.Rd"
      [10] "v1/subdir/dotenv/man/load_dot_env.Rd"  
      [11] "v1/wipe.R"                             

