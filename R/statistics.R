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

#' Create a streamer for calculating the exponential moving average
#'
#' @description \code{EMA} creates a streaming algorithm that can be used to
#' calculate the exponential moving average of incoming values
#'
#'
#' @docType class
#'
#' @examples
#' exp_mean <- EMA$new(c(1, 2))
#' exp_mean$update(c(3, 4))
#' exp_mean$value
#' #> [1] 3.266667
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
EMA <- R6::R6Class("EMA", public = list(
    #' @description Creates a new \code{EMA} streamer object.
    #'
    #' @param x values to be used during initialization (optional)
    #' @param alpha value of the smoothing factor (0.5 by default)
    #'
    #' @examples
    #' exp_mean <- EMA$new()
    #'
    #' @return The new \code{EMA} (invisibly)
    initialize = function(x = NULL, alpha = 0.5) {
        if (alpha < 0 || alpha > 1) {
            stop("The smoothing factor must take values between 0 and 1.")
        } else {
            private$alpha <- alpha
        }
        if (!is.null(x)) {
            private$update_values(x)
        }
        invisible(self)
    },
    #' @description Returns the current value of the EMA.
    #'
    #' @examples
    #' exp_mean <- EMA$new(x = c(1,2,3,4))
    #' exp_mean$value()
    #' #> [1] 3.266667
    #'
    #' @return The updated \code{EMA} (invisibly)
    value = function() {
        if (private$weighted_count == 0L) {
            return(0)
        }
        as.numeric(private$weighted_sum / private$weighted_count)
    },
    #' @description Updates the EMA with a stream of new values
    #'
    #' @param x a vector of values to update the EMA
    #'
    #' @examples
    #' exp_mean <- EMA$new(c(1,2))
    #' exp_mean$update(c(3,4))
    #'
    #' @return The updated \code{EMA} (invisibly)
    update = function(x) {
        private$update_values(x)
        invisible(self)
    }
), private = list(
    weighted_sum = 0,
    weighted_count = 0L,
    count = 0L,
    alpha = NULL,
    update_values = function(x) {
        k <- length(x)
        valpha <- rep((1 - private$alpha), k) ** seq(k - 1, 0)
        private$weighted_sum <- valpha %*% x + (1 - private$alpha)**k * private$weighted_sum
        private$count <- private$count + k
        private$weighted_count <- (1 - (1 - private$alpha)**private$count) / private$alpha
    }
)
)

