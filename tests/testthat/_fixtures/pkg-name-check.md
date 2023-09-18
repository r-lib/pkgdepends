# print.pkg_name_check

    Hash
      01ca05e7e503765b871858b0a893ae85
    Code
      {
          list(pkg_name_check("tools"), pkg_name_check("tools", dictionaries = "urban"))
      }

# format.pkg_name_basics

    Hash
      aa4b0a131ad85ee937ac2fa09743c560
    Code
      {
          list(sy(async_pnc_basics("pwr")), sy(async_pnc_basics(paste0("s", 
              "h", "i", "t"))))
      }

# format.pkg_name_check_wikipedia

    Hash
      1e01504bf366a3d1d8a30a1bbfe42923
    Code
      {
          list(sy(async_wikipedia_get("cli")), sy(async_wikipedia_get("surely-not-this")), 
              sy(async_wikipedia_get("GNU R")))
      }

# async_wiktionary_get

    Hash
      976e3d01a855e39ebd71c60482a1cac9
    Code
      {
          sy(async_wiktionary_get_query("foobar"))
      }

# wiktionary_get_process

    Hash
      976e3d01a855e39ebd71c60482a1cac9
    Code
      {
          sy(async_wiktionary_get_query("foobar"))
      }

# format.pkg_name_check_wiktionary

    Hash
      976e3d01a855e39ebd71c60482a1cac9
    Code
      {
          sy(async_wiktionary_get_query("foobar"))
      }

---

    Hash
      936aece313cdae39b02cde924e52eeb9
    Code
      {
          sy(async_wiktionary_get_query("not-at-all-sdfsdfsdf"))
      }

# async_profanity_get

    Hash
      51e4c0280615fb2fc232bea02081489b
    Code
      {
          sy(async_profanity_get_query("nope"))
      }

# profanity_get_process

    Hash
      51e4c0280615fb2fc232bea02081489b
    Code
      {
          sy(async_profanity_get_query("nope"))
      }

# async_urban_get

    Hash
      13205944297f7f4f50e756b13397c0e5
    Code
      {
          sy(async_urban_get_query("tool"))
      }

# urban_get_process

    Hash
      13205944297f7f4f50e756b13397c0e5
    Code
      {
          sy(async_urban_get_query("tool"))
      }

# format.pkg_name_check_urban

    Hash
      0468c02ed8cc959d7380ea172a55e336
    Code
      {
          sy(async_urban_get("tool"))
      }

---

    Hash
      c6ad8510dbbba26e1ee8e9e21bcee98f
    Code
      {
          sy(async_urban_get("not-this-one-asdfsf"))
      }

# async_pnc_bioc_web

    Hash
      4e90ad0d1fde0217f9683ea1f804af97
    Code
      {
          sy(async_pnc_bioc_query("all"))
      }

# pnc_bioc_process

    Hash
      4e90ad0d1fde0217f9683ea1f804af97
    Code
      {
          sy(async_pnc_bioc_query("all"))
      }

