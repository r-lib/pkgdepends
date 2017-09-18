
#' @importFrom R6 R6Class
#' @importFrom async deferred is_deferred

deferred_pool <- R6Class(
  "deferred_pool",
  public = list(
    initialize = function() {
      private$def <- deferred$new(function(resolve, reject) {
        private$resolve <- resolve
        private$reject <- reject
      })
      invisible(self)
    },

    add = function(deferred) {
      assert_that(is_deferred(deferred))
      if (private$done) stop("Deferred pool has already completed")
      private$num <- private$num + 1

      deferred$finally(function() {
        private$num <- private$num - 1
        if (private$finished && private$num == 0) {
          private$done <- TRUE
          private$resolve(TRUE)
        }
      })
      invisible(self)
    },

    finish = function() {
      private$finished <- TRUE
      if (!private$done && private$num == 0) {
        private$done <- TRUE
        private$resolve(TRUE)
      }
      invisible(self)
    },

    when_complete = function() private$def
  ),

  private = list(
    def      = NULL,
    resolve  = NULL,
    reject   = NULL,
    finished = FALSE,
    done     = FALSE,
    num      = 0
  )
)
