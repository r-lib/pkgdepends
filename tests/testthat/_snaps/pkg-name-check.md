# async_pkg_name_check

    Code
      sy(async_pkg_name_check(11))
    Condition
      Error:
      ! `name` must be a string (character scalar), but it is a number.
    Code
      sy(async_pkg_name_check(c("a", "b")))
    Condition
      Error:
      ! `name` must be a string (character scalar), but it is a character vector.
    Code
      sy(async_pkg_name_check(NA_character_))
    Condition
      Error:
      ! `name` must not be `NA`.
    Code
      sy(async_pkg_name_check("x", 11))
    Condition
      Error:
      ! One of `is.null(dictionaries)` and `is_character(dictionaries)` must be true.
    Code
      sy(async_pkg_name_check("x", NA_character_))
    Condition
      Error:
      ! One of `is.null(dictionaries)` and `is_character(dictionaries)` must be true.

# print.pkg_name_check

    Code
      print(ans[[1]])
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- tools --*--                              |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
      + Wikipedia -------------------------------------------------------------------+
      | Tool (from Tools) A tool is an object that can extend an individual's        |
      | ability to modify features of the surrounding environment or help them       |
      | accomplish a particular task. Although many animals use simple tools, only   |
      | human beings, whose use of stone tools dates back hundreds of millennia,     |
      | have been observed using tools to make other tools.                          |
      |                                                                              |
      | ...                                                                          |
      +------------------------------------------ https://en.wikipedia.org/wiki/Tool +
      + Wiktionary ------------------------------------------------------------------+
      | tools Noun: tools                                                            |
      | plural of tool                                                               |
      | Verb: tools                                                                  |
      | third-person singular simple present indicative form of tool                 |
      | Anagrams: loots, lotos, sloot, sotol, stool, tosol                           |
      +---------------------------------------- https://en.wiktionary.org/wiki/tools +
      +------------------------------------------------------------------------------+
      | Sentiment: :| (0)                                                            |
      +------------------------------------------------------------------------------+

---

    Code
      print(ans[[2]])
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- tools --*--                              |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
      + Urban dictionary ------------------------------------------------------------+
      | slang for the [hypodermic] [needles] used to [inject] drugs.                 |
      +-------------------------------------------- http://tools.urbanup.com/1443484 +

# pnc_base

    Code
      pnc_base("base")
    Output
      $base
      [1] FALSE
      
      $package
      [1] "base"
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

---

    Code
      pnc_base("Base")
    Output
      $base
      [1] FALSE
      
      $package
      [1] "base"
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

---

    Code
      pnc_base("TOOLS")
    Output
      $base
      [1] FALSE
      
      $package
      [1] "tools"
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

---

    Code
      pnc_base("definitely-not")
    Output
      $base
      [1] TRUE
      
      $package
      NULL
      
      attr(,"class")
      [1] "pkg_name_check_base" "list"               

# format.pkg_name_basics

    Code
      writeLines(format(bss[[1]]))
    Output
      +------------------------------------------------------------------------------+
      |                                --*-- pwr --*--                               |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      x  CRAN            v  Bioconductor    v  not a profanity  |
      +------------------------------------------------------------------------------+
    Code
      writeLines(format(bss[[2]]))
    Output
      +------------------------------------------------------------------------------+
      |                               --*-- shit --*--                               |
      +------------------------------------------------------------------------------+
      +------------------------------------------------------------------------------+
      | v  valid name      v  CRAN            v  Bioconductor    x  profanity        |
      +------------------------------------------------------------------------------+

# async_wikipedia_get_query

    Code
      show_request(ret)
    Output
      POST application/x-www-form-urlencoded
      Query string: 
      Body: action=query&format=json&prop=extracts&titles=foobar&redirects=1&exintro=1&explaintext=1

# format.pkg_name_check_wikipedia

    Code
      writeLines(format(wpd[[1]]))
    Output
      + Wikipedia -------------------------------------------------------------------+
      | CLI (from Cli) CLI may refer to multiple articles, see link.                 |
      +------------------------------------------- https://en.wikipedia.org/wiki/CLI +
    Code
      writeLines(format(wpd[[2]]))
    Output
      + Wikipedia -------------------------------------------------------------------+
      | Surely-not-this No definition found                                          |
      +------------------------------------------------------------------------------+
    Code
      writeLines(format(wpd[[3]]))
    Output
      + Wikipedia -------------------------------------------------------------------+
      | R (programming language) (from GNU R) R is a programming language for        |
      | statistical computing and graphics supported by the R Core Team and the R    |
      | Foundation for Statistical Computing. Created by statisticians Ross Ihaka    |
      | and Robert Gentleman, R is used among data miners, bioinformaticians and     |
      | statisticians for data analysis and developing statistical software. The     |
      | core R language is augmented by a large number of extension packages         |
      | ...                                                                          |
      +---------------------- https://en.wikipedia.org/wiki/R_(programming_language) +

# async_wiktionary_get

    Code
      print(ans)
    Output
      + Wiktionary ------------------------------------------------------------------+
      | foobar Etymology: Phonetic spelling of FUBAR, which is either an acronym     |
      | for "Fucked up beyond all recognition", or derived from foo.                 |
      | Noun: foobar (plural foobars)                                                |
      | (slang) A serious mistake. (programming) A metasyntactic variable name, a    |
      | place holder for words; compare foo, bar.                                    |
      | foo foo fighter FUBAR                                                        |
      | ...                                                                          |
      +--------------------------------------- https://en.wiktionary.org/wiki/foobar +

# async_wiktionary_get_query

    Code
      show_request(ret)
    Output
      POST application/x-www-form-urlencoded
      Query string: 
      Body: action=query&format=json&prop=extracts&titles=foobar&redirects=1&explaintext=1

# format.pkg_name_check_wiktionary

    Code
      writeLines(format(ans))
    Output
      + Wiktionary ------------------------------------------------------------------+
      | foobar Etymology: Phonetic spelling of FUBAR, which is either an acronym     |
      | for "Fucked up beyond all recognition", or derived from foo.                 |
      | Noun: foobar (plural foobars)                                                |
      | (slang) A serious mistake. (programming) A metasyntactic variable name, a    |
      | place holder for words; compare foo, bar.                                    |
      | foo foo fighter FUBAR                                                        |
      | ...                                                                          |
      +--------------------------------------- https://en.wiktionary.org/wiki/foobar +
    Code
      writeLines(format(ans2))
    Output
      + Wiktionary ------------------------------------------------------------------+
      | not-at-all-sdfsdfsdf No English definition found                             |
      +------------------------------------------------------------------------------+

# async_profanity_get_query

    Code
      show_request(ret)
    Output
      GET 
      Query string: text=foobar
      Body: 

# format.pkg_name_check_sentiment

    Code
      writeLines(format(ans1))
    Output
      +------------------------------------------------------------------------------+
      | Sentiment: :D (3)                                                            |
      +------------------------------------------------------------------------------+
    Code
      writeLines(format(ans2))
    Output
      +------------------------------------------------------------------------------+
      | Sentiment: :| (0)                                                            |
      +------------------------------------------------------------------------------+

# async_urban_get

    Code
      ans
    Output
      + Urban dictionary ------------------------------------------------------------+
      | One who lacks the mental capacity to know he is [being used].  A fool.  A    |
      | [cretin].  Characterized by low intelligence and/or [self-steem].            |
      +----------------------------------------------- http://tool.urbanup.com/38616 +

# async_urban_get_query

    Code
      show_request(ret)
    Output
      GET 
      Query string: term=foobar
      Body: 

# urban_get_process

    Code
      ans
    Output
      + Urban dictionary ------------------------------------------------------------+
      | One who lacks the mental capacity to know he is [being used].  A fool.  A    |
      | [cretin].  Characterized by low intelligence and/or [self-steem].            |
      +----------------------------------------------- http://tool.urbanup.com/38616 +

# format.pkg_name_check_urban

    Code
      writeLines(format(ans))
    Output
      + Urban dictionary ------------------------------------------------------------+
      | One who lacks the mental capacity to know he is [being used].  A fool.  A    |
      | [cretin].  Characterized by low intelligence and/or [self-steem].            |
      +----------------------------------------------- http://tool.urbanup.com/38616 +

---

    Code
      writeLines(format(ans2))
    Output
      + Urban dictionary ------------------------------------------------------------+
      | No definition found.                                                         |
      +------------------------------------------------------------------------------+

---

    Code
      writeLines(format(ans))
    Output
      + Urban dictionary ------------------------------------------------------------+
      | One who lacks the mental capacity to know he is [being used].  A fool.  A    |
      | [cretin].  Characterized by low intelligence and/or [self-steem]. One who    |
      | lacks the mental capacity to know he is [being used].  A fool.  A [cretin].  |
      | Characterized by low intelligence and/or [self-steem]. One who lacks the     |
      | mental capacity to know he is [being used].  A fool.  A [cretin].            |
      | Characterized by low intelligence and/or [self-steem]. One who lacks the     |
      | ...                                                                          |
      +----------------------------------------------- http://tool.urbanup.com/38616 +

# async_pnc_bioc_web

    Code
      ans
    Output
      $bioc
      [1] FALSE
      
      $package
      [1] "ALL"
      
      attr(,"class")
      [1] "pkg_name_check_bioc" "list"               

# pnc_bioc_query

    Code
      ans
    Output
      $bioc
      [1] FALSE
      
      $package
      [1] "ALL"
      
      attr(,"class")
      [1] "pkg_name_check_bioc" "list"               

---

    Code
      ans2
    Output
      $bioc
      [1] FALSE
      
      $package
      [1] "agcdf"
      
      attr(,"class")
      [1] "pkg_name_check_bioc" "list"               

---

    Code
      ans3
    Output
      $bioc
      [1] TRUE
      
      $package
      NULL
      
      attr(,"class")
      [1] "pkg_name_check_bioc" "list"               

# pnc_bioc_parse

    Code
      pkg1
    Output
      [1] "a4"          "a4Base"      "a4Classif"   "a4Core"      "a4Preproc"  
      [6] "a4Reporting" "aCGH"        "abseqR"      "ag.db"      
      attr(,"na.action")
      [1] 1 2
      attr(,"class")
      [1] "omit"

---

    Code
      pkg2
    Output
       [1] "ABAData"       "ABAEnrichment" "ABSSeq"        "AGDEX"        
       [5] "AHPathbankDbs" "AIMS"          "ALDEx2"        "ALL"          
       [9] "ALLMLL"        "ALPS"          "AMARETTO"     
      attr(,"na.action")
      [1] 1 2
      attr(,"class")
      [1] "omit"

# pnc_bioc_parse_pgz

    Code
      pkg
    Output
      [1] "adme16cod.db" "ag.db"        "agcdf"       

