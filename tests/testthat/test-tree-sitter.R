test_that("s_expr", {
  expect_snapshot(
    s_expr("f(arg, arg2 = 2); 1:100")
  )
})

test_that("code_query", {
  expect_snapshot({
    code_query("f(arg, arg2)", "(call(arguments))")
    code_query("f(arg, arg2)", "(call(arguments)) @call")
  })
})

test_that("code_query, multiple patterns", {
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

test_that("code_query, field names", {
  expect_snapshot({
    code_query(
      "f(arg, x = arg2)",
      "(argument name: (identifier) @name value: (identifier) @value)"
    )
  })
})

test_that("code_query, negated fields", {
  expect_snapshot({
    # function calls without arguments
    code_query(
      "f(arg1, x = arg2); g(); a",
      "((call (arguments !argument)) @call-name)"
    )
  })
})

test_that("code_query, anonymous nodes", {
  expect_snapshot({
    code_query("1 + 2", '((float) @lhs "+" @op (float)) @rhs')
  })
})

test_that("code_query, capturing nodes", {
  expect_snapshot({
    # function calls
    code_query(
      "library(testthat); foo(bar); library(pak); bar(baz)",
      "(call function: (identifier) @fun-name)"
    )
  })
})

test_that("code_query, quantification operators", {
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
  expect_snapshot({
    code_query(
      "f(); g(); h()",
      "((call) @call-1 (call) @call-2 (call) @call-3)"
    )
  })
})

test_that("code_query, alternations", {
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
  expect_snapshot({
    # anything in a loop sequence
    code_query(
      "for (i in 1:10) foo",
      "(for_statement sequence: (_) @seq)"
    )
  })
})

test_that("code_query, anchors", {
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

test_that("code_query, predicates, eq capture(1) x capture(1)", {
  expect_snapshot({
    # repeated function call
    code_query(
      "f(x); f(y); g()",
      paste0(
        "((call function: (identifier) @fun-name) ",
        "(call function: (identifier) @fun-name2) ",
        "(#eq? @fun-name @fun-name2))"
      )
    )
  })
})

test_that("code_query, predicates, eq capture(1) x capture(0)", {
  expect_snapshot({
    # TODO
    NULL
  })
})

test_that("code_query, predicates, eq capture(1) x capture(n)", {
  expect_snapshot({
    # TODO
    NULL
  })
})

test_that("code_query, predicates, eq capture(0) x capture(1)", {
  expect_snapshot({
    # TODO
    NULL
  })
})

test_that("code_query, predicates, eq capture(n) x capture(1)", {
  expect_snapshot({
    # TODO
    NULL
  })
})

test_that("code_query, predicates, eq capture(n) x capture(n)", {
  expect_snapshot({
    # TODO
    NULL
  })
})

test_that("pattern names", {
  expect_snapshot({
    code_query("f('x')", c("(call) (call)", "(call)"))[["patterns"]]
    code_query("f('x')", c(a = "(call) (call)", "(call)"))[["patterns"]]
    code_query("f('x')", c("(call) (call)", b = "(call)"))[["patterns"]]
    code_query("f('x')", c(a = "(call) (call)", b = "(call)"))[["patterns"]]
  })
})

test_that("syntax error is handled", {
  expect_snapshot({
    code_query("f(1); g(1,2); 1+; h(3)", "(call) @call-code")
  })
})