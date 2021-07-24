#' Create a streamer for calculating a running mean
#'
#' @description \code{Mean} creates a streaming algorithm that can be used to
#' keep track of the mean of incoming values.
#'
#'
#' @docType class
#'
#' @examples
#' mean <- Mean$new(c(1, 2))
#' mean$update(c(3, 4))
#' mean$value
#' #> [1] 2.5
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
Mean <- R6::R6Class("Mean", public = list(
    #' @description Creates a new \code{Mean} streamer object.
    #'
    #' @param x values to be used during initialisation (optional)
    #'
    #' @examples
    #' mean <- Mean$new()
    #'
    #' @return The new \code{Mean} (invisibly)
    initialize = function(x = NULL) {
        if (!is.null(x)) {
            private$sum <- private$sum + sum(x)
            private$count <- private$count + length(x)
        }
        invisible(self)
    },
    #' @description Resets the \code{Mean} streamer object.
    #'
    #' @param x values to be added to the stream
    #'
    #' @examples
    #' mean <- Mean$new()
    #' mean$update(c(1, 2))
    #'
    #' @return The updated \code{Mean} (invisibly)
    update = function(x) {
        private$sum <- private$sum + sum(x)
        private$count <- private$count + length(x)
        invisible(self)
    },
    #' @description Returns the current value of the mean.
    #'
    #' @examples
    #' mean <- Mean$new(c(1, 2, 3))
    #' mean$value()
    #' #> [1] 2
    #'
    #' @return The updated \code{Mean} (invisibly)
    value = function() {
        if (private$count == 0L) {
            return(0)
        }
        private$sum / private$count
    }
    ), private = list(
        sum = 0,
        count = 0L
    )
)
