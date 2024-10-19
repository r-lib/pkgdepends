# match any library(), require(), base::library(), base::require(), etc. calls
# we use these as fallbacks. If a call is not identified some other way
# we parse it with R and match the call.
q_library_0 <- function() {
  structure(c(
    '((call function: (identifier) @fn-name) @dep-code
      (#any-of? @fn-name "library" "require" "loadNamespace" "requireNamespace"))',
    '((call function:
       (namespace_operator
        lhs: (identifier) @ns-name
        rhs: (identifier) @fn-name
       )
      ) @dep-code
      (#eq? @ns-name "base")
      (#any-of? @fn-name "library" "require" "loadNamespace" "requireNamespace"))'
  ), names = rep("q_library_0", 2))
}

# library(foo), require(foo), base::library(foo), base::require(foo)
# - not used currently, we match every call with R
# - function call is library or require
# - the only argument (. argument .) is not named (. value:)
# - the only argument is a symbol or a string
# - this should catch most of the usage, the rest we check with
#   `match.call()`
q_library_1 <- function() {
  '((call function: [
      (identifier) @fn-name
      (namespace_operator lhs: (identifier) @ns-name rhs: (identifier) @fn-name)
    ]
    arguments: (arguments . argument: (argument . value: [
      (identifier) @pkg-name
      (string (string_content) @pkg-name)
    ]) . )
    (#eq? @ns-name "base")
    (#any-of? @fn-name "library" "require")
  ) @dep-code)'
}

# pkg::fun, pkg:::fun
q_colon <- function() {
  '((namespace_operator lhs: (identifier) @pkg-name) @dep-code
    (#not-eq? @pkg-name "base")
  )'
}

# requireNamespace("foo"), loadNamespace("foo")
# - not used currently, we match every call with R
# - first argument is unnamed
# - first argument is a string
# - we can parse the other cases with R
q_require_namespace <- function() {
  '((call function: (identifier) @fn-name
    arguments: (arguments . argument: (argument . value:
      (string (string_content) @pkg-name)
    ))
    (#any-of? @fn-name "loadNamespace" "requireNamespace")
  ) @dep-code)'
}

q_deps <- function() {
  c(
    q_library_0(),
    q_colon(),
    NULL
  )
}
