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

# pkg::fun, pkg:::fun
q_colon <- function() {
  '((namespace_operator lhs: (identifier) @pkg-name) @dep-code
    (#not-eq? @pkg-name "base")
  )'
}

q_deps <- function() {
  c(
    q_library_0(),
    q_colon(),
    NULL
  )
}
