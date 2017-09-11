
httpbin_url <- function() {
  "eu.httpbin.org"
}

is_offline <- (function() {
  offline <- NULL
  function() {
    if (is.null(offline)) {
      offline <<- tryCatch(
        is.na(pingr::ping_port(httpbin_url(), port = 443, count = 1L)),
        error = function(e) TRUE
      )
    }
    offline
  }
})()

skip_if_offline <- function() {
  if (is_offline()) skip("Offline")
}

expect_equal_named_lists <- function(object, expected, ...) {
  expect_true(!is.null(names(object)) && !is.null(names(expected)))
  expect_true(is.list(object) && is.list(expected))
  object <- object[order(names(object))]
  expected <- expected[order(names(expected))]
  expect_equal(object, expected)
}
