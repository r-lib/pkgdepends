
context("deferred_pool")

test_that("deferred_pool", {

  done <- 0
  complete <- FALSE

  afun <- async(function() {
    pool <- deferred_pool$new()
    on.exit(pool$finish())
    pool$add(d1 <- delay(1/1000)$then(function() done <<- done + 1))
    pool$add(d2 <- delay(1/1000)$then(function() done <<- done + 1))
    px <- pool$when_complete()$then(function() complete <<- TRUE)
    pool$finish()
    px
  })

  synchronise(afun())

  expect_equal(done, 2)
  expect_true(complete)
})

test_that("deferred_pool finish when nothing is running (any more)", {

  done <- 0
  complete <- FALSE

  afun <- async(function() {
    pool <- deferred_pool$new()
    on.exit(pool$finish())
    pool$add(d1 <- delay(1/1000)$then(function() done <<- done + 1))
    px <- pool$when_complete()$then(function() complete <<- TRUE)
    await(d1)
    pool$finish()
    px
  })

  synchronise(afun())

  expect_equal(done, 1)
  expect_true(complete)
})
