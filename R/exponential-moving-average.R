ExpMean <- R6::R6Class("ExpMean", publi = list(
    #' @description Creates a new \code{ExpMean} streamer object.
    #'
    #' @param x values to be used during initialisation (optional)
    #' @param alpha value of the smoothing factor (0.5 by default)
    #'
    #' @examples
    #' mean <- ExpMean$new()
    #' mean$reset()
    #'
    #' @return The new \code{ExpMean} (invisibly)
    initialize = function(x = NULL, alpha = 0.5) {
        if(alpha < 0 || alpha > 1) {
            stop("The smoothing factor must take values between 0 and 1.")
        } else {
            private$alpha <- alpha
            if(is.null(x)) {
                private$mean <- NULL
            } else {
                k <- length(x)
                if(k == 1) {
                    private$mean <- x
                } else {
                    coefs <- rep((1-private$alpha), k-1) ** seq(k - 2, 0)
                    private$mean <- private$alpha * (coefs %*% x[2:k]) + (1 - private$alpha)**(k-1) * x[1]
                }
            }
        }
        invisible(self)
    },
    #' @description Updated the mean by a single value
    #'
    #' @param x single vaule to be updated in the mean
    #'
    #' @examples
    #' mean <- ExpMean$new(c(1,2))
    #' mean$update(c(3,4))
    #'
    #' @return The updated \code{ExpMean} (invisibly)
    update = function(x) {
        if(is.null(private$mean)) {
            k <- length(x)
            if(k == 1) {
                private$mean <- x
            } else {
                coefs <- rep((1-private$alpha), k-1) ** seq(k - 2, 0)
                private$mean <- private$alpha * (coefs %*% x[2:k]) + (1 - private$alpha)**(k-1) * x[1]
            }
        } else {
            k <- length(x)
            coefs <- rep((1-private$alpha), k) ** seq(k - 1, 0)
            private$mean <- private$alpha * (coefs %*% x) + (1 - private$alpha)**k * private$mean
        }
        invisible(self)
    },
    #' @description Returns the current value of the mean.
    #'
    #' @examples
    #' mean <- ExpMean$new(x = c(1,2,3,4))
    #' mean$value()
    #' #> [1] 3.125
    #'
    #' @return The updated \code{Mean} (invisibly)
    value = function() {
        private$mean
    },
    #' @description Resets the \code{ExpMean} streamer object.
    #'
    #' @param x values to be used during re-initialisation (optional)
    #' @param alpha value of the smoothing factor (0.5 by default)
    #'
    #' @examples
    #' mean <- Mean$new()
    #' mean$reset()
    #'
    #' @return The updated \code{ExpMean} (invisibly)
    reset = function(x = NULL, alpha = 0.5) {
        private$alpha <- alpha
        if (!is.null(x)) {

        } else {
            private$sum <- 0
            private$count <- 0L
        }
        invisible(self)
    }
    ), private = list(
        mean = NULL,
        alpha = 0.5
    )
)
