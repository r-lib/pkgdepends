
context("deferred_pool")

test_that("deferred_pool", {
  pool <- deferred_pool$new()
  on.exit(pool$finish())
  done <- 0
  complete <- FALSE
  pool$add(d1 <- delay(1/1000)$then(function() done <<- done + 1))
  pool$add(d2 <- delay(1/1000)$then(function() done <<- done + 1))
  px <- pool$when_complete()$then(function() complete <<- TRUE)
  pool$finish()
  await(px)

  expect_equal(done, 2)
  expect_true(complete)
})
