# match any library() and require calls
# we use these as fallbacks. If a call is not identified some other way
# we parse it with R and match the call.
q_library_0 <- function() {
  '((call function: (identifier) @fn-name) @dep-code
    (#any-of? @fn-name "library" "require")
  )'
}

# library(foo)
# - function call is library or require
# - the only argument (. argument .) is not named (. value:)
# - the only argument is a symbol or a string
# - this should catch most of the usage, the rest we check with
#   `match.call()`
q_library_1 <- function() {
  '(call function: (identifier) @fn-name
    arguments: (arguments . argument: (argument . value: [
      (identifier) @pkg-name
      (string (string_content) @pkg-name)
    ]) . )
    (#any-of? @fn-name "library" "require")
  ) @dep-code'
}

q_colon <- function() {
  '(namespace_operator lhs: (identifier) @pkg-name) @dep-code'
}

q_deps <- function() {
  c(
    q_library_0 = q_library_0(),
    q_library_1 = q_library_1(),
    q_colon = q_colon()
  )
}
