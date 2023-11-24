# write_str vectors

    Code
      tojson$write_str(NULL)
    Output
      [1] "{}"
    Code
      tojson$write_str(list())
    Output
      [1] "[]"
    Code
      tojson$write_str(integer())
    Output
      [1] "[]"
    Code
      tojson$write_str(double())
    Output
      [1] "[]"
    Code
      tojson$write_str(character())
    Output
      [1] "[]"
    Code
      tojson$write_str(logical())
    Output
      [1] "[]"

---

    Code
      tojson$write_str(complex())
    Condition
      Error:
      ! Cannot convert atomic complex vectors to JSON.
    Code
      tojson$write_str(raw())
    Condition
      Error:
      ! Cannot convert atomic raw vectors to JSON.

---

    Code
      tojson$write_str(list(1))
    Output
      [1] "[[1]]"
    Code
      tojson$write_str(1L)
    Output
      [1] "[1]"
    Code
      tojson$write_str(1)
    Output
      [1] "[1]"
    Code
      tojson$write_str("foo")
    Output
      [1] "[\"foo\"]"
    Code
      tojson$write_str(TRUE)
    Output
      [1] "[true]"

---

    Code
      tojson$write_str(list(1, 2))
    Output
      [1] "[[1],[2]]"
    Code
      tojson$write_str(1:2)
    Output
      [1] "[1,2]"
    Code
      tojson$write_str(c(1, 2))
    Output
      [1] "[1,2]"
    Code
      tojson$write_str(c("foo", "bar"))
    Output
      [1] "[\"foo\",\"bar\"]"
    Code
      tojson$write_str(c(TRUE, FALSE))
    Output
      [1] "[true,false]"

---

    Code
      tojson$write_str(NA_integer_)
    Output
      [1] "[null]"
    Code
      tojson$write_str(NA_real_)
    Output
      [1] "[null]"
    Code
      tojson$write_str(NA_character_)
    Output
      [1] "[null]"
    Code
      tojson$write_str(NA)
    Output
      [1] "[null]"

---

    Code
      tojson$write_str(c(1L, NA_integer_))
    Output
      [1] "[1,null]"
    Code
      tojson$write_str(c(1, NA_real_))
    Output
      [1] "[1,null]"
    Code
      tojson$write_str(c("foo", NA_character_))
    Output
      [1] "[\"foo\",null]"
    Code
      tojson$write_str(c(TRUE, NA))
    Output
      [1] "[true,null]"

---

    Code
      tojson$write_str(c(a = 1L, b = 2L))
    Output
      [1] "[1,2]"
    Code
      tojson$write_str(c(a = 1, b = 2))
    Output
      [1] "[1,2]"
    Code
      tojson$write_str(c(a = "foo", b = "bar"))
    Output
      [1] "[\"foo\",\"bar\"]"
    Code
      tojson$write_str(c(a = TRUE, b = FALSE))
    Output
      [1] "[true,false]"

# write_str character encoding and escaping

    Code
      cat(tojson$write_str("foo\"\\bar"))
    Output
      ["foo\"\\bar"]
    Code
      charToRaw(tojson$write_str(iconv(utf8, "UTF-8", "latin1")))
    Output
       [1] 5b 22 47 c3 a1 62 6f 72 22 5d

# lists

    Code
      cat(tojson$write_str(list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))))
    Output
      [[[1],[2],[3]],[[4],[5],[6]],[[7],[8],[9]]]

---

    Code
      cat(tojson$write_str(list(a = 1, b = 2)))
    Output
      {"a":[1],"b":[2]}

---

    Code
      cat(tojson$write_str(list(a = list(a1 = 1, a2 = 2), b = list(b1 = 3, b2 = 4))))
    Output
      {"a":{"a1":[1],"a2":[2]},"b":{"b1":[3],"b2":[4]}}

---

    Code
      cat(tojson$write_str(list(a = list(1, a2 = 2), list(b1 = 3, 4))))
    Output
      {"a":{"1":[1],"a2":[2]},"2":{"b1":[3],"2":[4]}}

# auto-unbox

    Code
      tojson$write_str(list(1), opts = list(auto_unbox = TRUE))
    Output
      [1] "[1]"
    Code
      tojson$write_str(1L, opts = list(auto_unbox = TRUE))
    Output
      [1] "1"
    Code
      tojson$write_str(1, opts = list(auto_unbox = TRUE))
    Output
      [1] "1"
    Code
      tojson$write_str("foo", opts = list(auto_unbox = TRUE))
    Output
      [1] "\"foo\""
    Code
      tojson$write_str(TRUE, opts = list(auto_unbox = TRUE))
    Output
      [1] "true"

---

    Code
      tojson$write_str(NA_integer_, opts = list(auto_unbox = TRUE))
    Output
      [1] "null"
    Code
      tojson$write_str(NA_real_, opts = list(auto_unbox = TRUE))
    Output
      [1] "null"
    Code
      tojson$write_str(NA_character_, opts = list(auto_unbox = TRUE))
    Output
      [1] "null"
    Code
      tojson$write_str(NA, opts = list(auto_unbox = TRUE))
    Output
      [1] "null"

---

    Code
      cat(tojson$write_str(list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9)), opts = list(
        auto_unbox = TRUE)))
    Output
      [[1,2,3],[4,5,6],[7,8,9]]

---

    Code
      cat(tojson$write_str(list(a = 1, b = 2), opts = list(auto_unbox = TRUE)))
    Output
      {"a":1,"b":2}

---

    Code
      cat(tojson$write_str(list(a = list(a1 = 1, a2 = 2), b = list(b1 = 3, b2 = 4)),
      opts = list(auto_unbox = TRUE)))
    Output
      {"a":{"a1":1,"a2":2},"b":{"b1":3,"b2":4}}

---

    Code
      cat(tojson$write_str(list(list(1, 2:3, 4), list(4:5, 6, 7:8), list(9:10, 11, 12)),
      opts = list(auto_unbox = TRUE)))
    Output
      [[1,[2,3],4],[[4,5],6,[7,8]],[[9,10],11,12]]

---

    Code
      cat(tojson$write_str(list(a = 1:3, b = 4), opts = list(auto_unbox = TRUE)))
    Output
      {"a":[1,2,3],"b":4}

---

    Code
      cat(tojson$write_str(list(a = list(a1 = 1, a2 = 2:3), b = list(b1 = 4:6, b2 = 7)),
      opts = list(auto_unbox = TRUE)))
    Output
      {"a":{"a1":1,"a2":[2,3]},"b":{"b1":[4,5,6],"b2":7}}

# unbox

    Code
      tojson$write_str(list(tojson$unbox(1)))
    Output
      [1] "[1]"
    Code
      tojson$write_str(tojson$unbox(1L))
    Output
      [1] "1"
    Code
      tojson$write_str(tojson$unbox(1))
    Output
      [1] "1"
    Code
      tojson$write_str(tojson$unbox("foo"))
    Output
      [1] "\"foo\""
    Code
      tojson$write_str(tojson$unbox(TRUE))
    Output
      [1] "true"

---

    Code
      tojson$write_str(tojson$unbox(NA_integer_))
    Output
      [1] "null"
    Code
      tojson$write_str(tojson$unbox(NA_real_))
    Output
      [1] "null"
    Code
      tojson$write_str(tojson$unbox(NA_character_))
    Output
      [1] "null"
    Code
      tojson$write_str(tojson$unbox(NA))
    Output
      [1] "null"

---

    Code
      cat(tojson$write_str(list(list(1, tojson$unbox(2), 3), list(tojson$unbox(4), 5,
      6), list(7, 8, 9))))
    Output
      [[[1],2,[3]],[4,[5],[6]],[[7],[8],[9]]]

---

    Code
      cat(tojson$write_str(list(a = 1, b = tojson$unbox(2))))
    Output
      {"a":[1],"b":2}

---

    Code
      cat(tojson$write_str(list(a = list(a1 = tojson$unbox(1), a2 = 2), b = list(b1 = 3,
        b2 = tojson$unbox(4)))))
    Output
      {"a":{"a1":1,"a2":[2]},"b":{"b1":[3],"b2":4}}

---

    Code
      tojson$unbox(list())
    Condition
      Error:
      ! Can only unbox atomic scalar, not list.
    Code
      tojson$unbox(1:2)
    Condition
      Error:
      ! Cannot unbox vector of length 2.
    Code
      tojson$unbox(double(2))
    Condition
      Error:
      ! Cannot unbox vector of length 2.
    Code
      tojson$unbox(character(2))
    Condition
      Error:
      ! Cannot unbox vector of length 2.
    Code
      tojson$unbox(logical(2))
    Condition
      Error:
      ! Cannot unbox vector of length 2.

# pretty

    Code
      tojson$write_str(1:5, opts = list(pretty = TRUE))
    Output
      [1] "[1, 2, 3, 4, 5]"
    Code
      tojson$write_str(1:5 / 2, opts = list(pretty = TRUE))
    Output
      [1] "[0.5, 1, 1.5, 2, 2.5]"
    Code
      tojson$write_str(letters[1:5], opts = list(pretty = TRUE))
    Output
      [1] "[\"a\", \"b\", \"c\", \"d\", \"e\"]"
    Code
      tojson$write_str(1:5 %% 2 == 0, opts = list(pretty = TRUE))
    Output
      [1] "[false, true, false, true, false]"

---

    Code
      cat(tojson$write_str(list(list(1, tojson$unbox(2), 3), list(tojson$unbox(4), 5,
      6), list(7, 8, 9)), opts = list(pretty = TRUE)))
    Output
      [
        [
          [1],
          2,
          [3]
        ],
        [
          4,
          [5],
          [6]
        ],
        [
          [7],
          [8],
          [9]
        ]
      ]

---

    Code
      cat(tojson$write_str(list(a = 1, b = tojson$unbox(2)), opts = list(pretty = TRUE)))
    Output
      {
        "a": [1],
        "b": 2
      }

---

    Code
      cat(tojson$write_str(list(a = list(a1 = tojson$unbox(1), a2 = 2), b = list(b1 = 3,
        b2 = tojson$unbox(4))), opts = list(pretty = TRUE)))
    Output
      {
        "a": {
          "a1": 1,
          "a2": [2]
        },
        "b": {
          "b1": [3],
          "b2": 4
        }
      }

# data frames

    Code
      tojson$write_str(df)
    Output
      [1] "[{\"mpg\":21,\"cyl\":6,\"disp\":160},{\"mpg\":21,\"cyl\":6,\"disp\":160},{\"mpg\":22.8,\"cyl\":4,\"disp\":108}]"
    Code
      cat(tojson$write_str(df, opts = list(pretty = TRUE)))
    Output
      [
        {
          "mpg": 21,
          "cyl": 6,
          "disp": 160
        },
        {
          "mpg": 21,
          "cyl": 6,
          "disp": 160
        },
        {
          "mpg": 22.8,
          "cyl": 4,
          "disp": 108
        }
      ]

---

    Code
      tojson$write_str(df)
    Output
      [1] "[{\"cyl\":6,\"disp\":160},{\"mpg\":21,\"cyl\":6},{\"mpg\":22.8,\"cyl\":4,\"disp\":108}]"
    Code
      cat(tojson$write_str(df, opts = list(pretty = TRUE)))
    Output
      [
        {
          "cyl": 6,
          "disp": 160
        },
        {
          "mpg": 21,
          "cyl": 6
        },
        {
          "mpg": 22.8,
          "cyl": 4,
          "disp": 108
        }
      ]

---

    Code
      tojson$write_str(df)
    Output
      [1] "[{\"cyl\":6,\"disp\":160,\"list\":[1,2]},{\"mpg\":21,\"cyl\":6,\"list\":[5]},{\"mpg\":22.8,\"cyl\":4,\"disp\":108,\"list\":[\"a\",\"b\",\"c\"]}]"
    Code
      cat(tojson$write_str(df, opts = list(pretty = TRUE)))
    Output
      [
        {
          "cyl": 6,
          "disp": 160,
          "list": [1, 2]
        },
        {
          "mpg": 21,
          "cyl": 6,
          "list": [5]
        },
        {
          "mpg": 22.8,
          "cyl": 4,
          "disp": 108,
          "list": ["a", "b", "c"]
        }
      ]

