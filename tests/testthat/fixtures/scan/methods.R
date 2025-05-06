## A simple class with two slots
track <- setClass("track", slots = c(x = "numeric", y = "numeric"))
## an object from the class
t1 <- track(x = 1:10, y = 1:10 + rnorm(10))

setGeneric("plot")

setMethod(
  "plot",
  signature(x = "track", y = "missing"),
  function(x, y, ...) plot(x@x, x@y, ...)
)
