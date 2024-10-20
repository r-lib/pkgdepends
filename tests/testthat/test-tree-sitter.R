test_that("s_expr", {
  local_reproducible_output(width = 500)
  expect_snapshot(
    s_expr("f(arg, arg2 = 2); 1:100")
  )
})

test_that("code_query", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query("f(arg, arg2)", "(call(arguments))")
    code_query("f(arg, arg2)", "(call(arguments)) @call")
  })
})

test_that("code_query, multiple patterns", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query(
      "f(arg, arg2)",
      "(call(arguments)) @call (arguments) @args"
    )
    code_query(
      "f(arg, arg2)",
      c("(call(arguments)) @call", "(arguments) @args")
    )
  })
})

test_that("pattern names", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query("f('x')", c("(call) (call)", "(call)"))[["patterns"]]
    code_query("f('x')", c(a = "(call) (call)", "(call)"))[["patterns"]]
    code_query("f('x')", c("(call) (call)", b = "(call)"))[["patterns"]]
    code_query("f('x')", c(a = "(call) (call)", b = "(call)"))[["patterns"]]
  })
})

test_that("syntax error is handled", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query("f(1); g(1,2); 1+; h(3)", "(call) @call-code")
  })
})
test_that("code_query, field names", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query(
      "f(arg, x = arg2)",
      "(argument name: (identifier) @name value: (identifier) @value)"
    )
  })
})

test_that("code_query, negated fields", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    # function calls without arguments
    code_query(
      "f(arg1, x = arg2); g(); a",
      "((call (arguments !argument)) @call-name)"
    )
  })
})

test_that("code_query, anonymous nodes", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query("1 + 2", '((float) @lhs "+" @op (float)) @rhs')
  })
})

test_that("code_query, capturing nodes", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    # function calls
    code_query(
      "library(testthat); foo(bar); library(pak); bar(baz)",
      "(call function: (identifier) @fun-name)"
    )
  })
})

test_that("code_query, quantification operators", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    # blocks of comments
    code_query(
      "# comment\n#comment 2\n1 + 5\n#comment 3\n",
      "(comment)+ @comments"
    )
  })
  expect_snapshot({
    # comments before function calls
    code_query(
      "f()\n# comment\ng()# comment1\n# comment 2\nh()\n",
      "((comment)* @comments (call function: (identifier)) @call)"
    )
  })
  expect_snapshot({
    # single optional comment before function call
    code_query(
      "f()\n# comment\ng()# comment1\n# comment 2\nh()\n",
      "((comment)? @comment (call function: (identifier)) @call)"
    )
  })
})

test_that("code_query, grouping sibling nodes", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    code_query(
      "f(); g(); h()",
      "((call) @call-1 (call) @call-2 (call) @call-3)"
    )
  })
})

test_that("code_query, alternations", {
  local_reproducible_output(width = 500)
  # named or unnamed argument, capture the arg name if any
  expect_snapshot({
    code_query(
      "f(x=1); f(1)",
      paste0(
        "(call function: (identifier) arguments: (arguments argument: [",
        "  (argument value: (_)) ",
        "  (argument name: (identifier) @arg-name value: (_))",
        "]))"
      )
    )
  })
})

test_that("code_query, wildcard node", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    # anything in a loop sequence
    code_query(
      "for (i in 1:10) foo",
      "(for_statement sequence: (_) @seq)"
    )
  })
})

test_that("code_query, anchors", {
  local_reproducible_output(width = 500)
  expect_snapshot({
    # single optional comment _right_ before function call
    code_query(
      "f()\n# comment\ng()# comment1\n# comment 2\nh()\n",
      "((comment)? @comment . (call function: (identifier)) @call)"
    )
  })
  expect_snapshot({
    # first argument
    code_query(
      "f(); f(x); f(x,y,z); f(x = 1, 2)",
      "(call arguments: (arguments . (argument) @arg))"
    )
  })
})

test_that("code_query, predicates, #eq?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program
      (call function: (identifier) @fn-1) %s
      (comment)
      (call function: (identifier) @fn-2) %s
      (#eq? @fn-1 @fn-2)
    )"
  do <- function(q1, q2, code) {
    q_ <- sprintf(q, q1, q2)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "", c("f()", "# c", "f()"))
    do("", "", c("f()", "# c", "g()"))
    # *
    do("*", "*", c("f()", "# c", "f()"))
    do("*", "*", c("f()", "# c", "g()"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "g(5)"))
    do("*", "*", c("f()", "# c"))
    do("*", "*", c("# c", "f()"))
    # +
    do("+", "+", c("f()", "# c", "f()"))
    do("+", "+", c("f()", "# c", "g()"))
    do("+", "+", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    do("+", "+", c("f()", "f(1)", "f(2)", "# c", "f()", "g(5)"))
    do("+", "+", c("f()", "# c"))
    do("+", "+", c("# c", "f()"))
    # ?
    do("?", "?", c("f()", "# c", "f()"))
    do("?", "?", c("f()", "# c", "g()"))
    do("?", "?", c("f()", "f(1)", "f(2)", "# c", "f(5)"))
    do("?", "?", c("f()", "# c", "f(5)", "f()"))
    do("?", "?", c("f()", "# c"))
    do("?", "?", c("# c", "f()"))
  })
})

test_that("code_query, predicates, #not-eq?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program
      (call function: (identifier) @fn-1) %s
      (comment)
      (call function: (identifier) @fn-2) %s
      (#not-eq? @fn-1 @fn-2)
    )"
  do <- function(q1, q2, code) {
    q_ <- sprintf(q, q1, q2)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "", c("f()", "# c", "f()"))
    do("", "", c("f()", "# c", "g()"))
    # *
    do("*", "*", c("f()", "# c", "f()"))
    do("*", "*", c("f()", "# c", "g()"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "h()", "g(5)"))
    do("*", "*", c("f()", "# c"))
    do("*", "*", c("# c", "f()"))
    # +
    do("+", "+", c("f()", "# c", "f()"))
    do("+", "+", c("f()", "# c", "g()"))
    do("+", "+", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    do("+", "+", c("f()", "f(1)", "f(2)", "# c", "h()", "g(5)"))
    do("+", "+", c("f()", "# c"))
    do("+", "+", c("# c", "f()"))
    # ?
    do("?", "?", c("f()", "# c", "f()"))
    do("?", "?", c("f()", "# c", "g()"))
    do("?", "?", c("f()", "f(1)", "f(2)", "# c", "g(5)"))
    do("?", "?", c("f()", "# c", "g(5)", "g()"))
    do("?", "?", c("f()", "# c"))
    do("?", "?", c("# c", "f()"))
  })
})

test_that("code_query, predicates, #any-eq?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program
      (call function: (identifier) @fn-1) %s
      (comment)
      (call function: (identifier) @fn-2) %s
      (#any-eq? @fn-1 @fn-2)
    )"
  do <- function(q1, q2, code) {
    q_ <- sprintf(q, q1, q2)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "", c("f()", "# c", "f()"))
    do("", "", c("f()", "# c", "g()"))
    # *
    do("*", "*", c("f()", "# c", "f()"))
    do("*", "*", c("f()", "# c", "g()"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    do("*", "*", c("f()", "# c"))
    do("*", "*", c("# c", "f()"))
    # +
    do("+", "+", c("f()", "# c", "f()"))
    do("+", "+", c("f()", "# c", "g()"))
    do("+", "+", c("f()", "f(1)", "g(2)", "# c", "f()", "f(5)"))
    do("+", "+", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    do("+", "+", c("f()", "# c"))
    do("+", "+", c("# c", "f()"))
    # ?
    do("?", "?", c("f()", "# c", "f()"))
    do("?", "?", c("f()", "# c", "g()"))
    do("?", "?", c("f()", "f(1)", "g(2)", "# c", "f(5)"))
    do("?", "?", c("g()", "# c", "f(5)", "f()"))
    do("?", "?", c("f()", "# c"))
    do("?", "?", c("# c", "f()"))
  })
})

test_that("code_query, predicates, #any-not-eq?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program
      (call function: (identifier) @fn-1) %s
      (comment)
      (call function: (identifier) @fn-2) %s
      (#any-not-eq? @fn-1 @fn-2)
    )"
  do <- function(q1, q2, code) {
    q_ <- sprintf(q, q1, q2)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "", c("f()", "# c", "f()"))
    do("", "", c("f()", "# c", "g()"))
    # *
    do("*", "*", c("f()", "# c", "f()"))
    do("*", "*", c("f()", "# c", "g()"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "f()", "f(5)"))
    do("*", "*", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    do("*", "*", c("f()", "# c"))
    do("*", "*", c("# c", "f()"))
    # +
    do("+", "+", c("f()", "# c", "f()"))
    do("+", "+", c("f()", "# c", "g()"))
    do("+", "+", c("f()", "f(1)", "g(2)", "# c", "f()", "f(5)"))
    do("+", "+", c("f()", "f(1)", "f(2)", "# c", "g()", "g(5)"))
    do("+", "+", c("f()", "# c"))
    do("+", "+", c("# c", "f()"))
    # ?
    do("?", "?", c("f()", "# c", "f()"))
    do("?", "?", c("f()", "# c", "g()"))
    do("?", "?", c("f()", "f(1)", "g(2)", "# c", "f(5)"))
    do("?", "?", c("g()", "# c", "f(5)", "f()"))
    do("?", "?", c("f()", "# c"))
    do("?", "?", c("# c", "f()"))
  })
})

test_that("code_query, predicates, #eq? vs string", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#eq? @fn \"f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #not-eq? vs string", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#not-eq? @fn \"f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("g()", "h()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("g()", "h()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("g()", "h()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #any-eq? vs string", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#any-eq? @fn \"f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #any-not-eq? vs string", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#any-not-eq? @fn \"f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("g()", "h()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("g()", "h()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("g()", "h()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #match?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#match? @fn \"^f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #not-match?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#not-match? @fn \"^f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("g()", "h()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("g()", "h()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("g()", "h()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #any-match?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#any-match? @fn \"^f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #any-not-match?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#any-not-match? @fn \"^f\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f()"))
    do("*", c("g()", "h()"))
    do("*", c("f()", "g()", "f()"))
    # +
    do("+", "")
    do("+", c("f()", "f()"))
    do("+", c("g()", "h()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f()"))
    do("?", c("g()", "h()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #any-of?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#any-of? @fn \"f\" \"f2\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f2()"))
    do("*", c("f()", "g()", "f2()"))
    # +
    do("+", "")
    do("+", c("f()", "f2()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f2()"))
    do("?", c("f()", "g()"))
  })
})

test_that("code_query, predicates, #not-any-of?", {
  local_reproducible_output(width = 500)
  # multiple calls to the same function, from before and after comment
  q <- "
    (program .
      (call function: (identifier) @fn) %s
      (#not-any-of? @fn \"f\" \"f2\")
    . )"
  do <- function(q1, code) {
    q_ <- sprintf(q, q1)
    code_query(code, q_)
  }
  expect_snapshot({
    # no auantification
    do("", "f()")
    do("", "g()")
    # *
    do("*", "")
    do("*", c("f()", "f2()"))
    do("*", c("g()", "h()"))
    do("*", c("f()", "g()", "f2()"))
    # +
    do("+", "")
    do("+", c("f()", "f2()"))
    do("+", c("g()", "h()"))
    do("+", c("f()", "g(1)"))
    # ?
    do("?", "")
    do("?", c("f()", "f2()"))
    do("?", c("g()", "h()"))
    do("?", c("f()", "g()"))
  })
})
