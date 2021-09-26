#' Create a streamer for calculating the cumulative moving average
#'
#' @description \code{CMA} creates a streaming algorithm that can be used to
#' keep track of the mean of incoming values.
#'
#'
#' @docType class
#'
#' @examples
#' mean <- CMA$new(c(1, 2))
#' mean$update(c(3, 4))
#' mean$value()
#' #> [1] 2.5
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
CMA <- R6::R6Class("CMA", public = list(
    #' @description Creates a new \code{CMA} streamer object.
    #'
    #' @param x values to be used during initialisation (optional)
    #'
    #' @examples
    #' mean <- CMA$new()
    #'
    #' @return The new \code{CMA} (invisibly)
    initialize = function(x = NULL) {
        if (!is.null(x)) {
            private$sum <- private$sum + sum(x)
            private$count <- private$count + length(x)
        }
        invisible(self)
    },
    #' @description Resets the \code{CMA} streamer object.
    #'
    #' @param x values to be added to the stream
    #'
    #' @examples
    #' mean <- CMA$new()
    #' mean$update(c(1, 2))
    #'
    #' @return The updated \code{CMA} (invisibly)
    update = function(x) {
        private$sum <- private$sum + sum(x)
        private$count <- private$count + length(x)
        invisible(self)
    },
    #' @description Returns the current value of the mean.
    #'
    #' @examples
    #' mean <- CMA$new(c(1, 2, 3))
    #' mean$value()
    #' #> [1] 2
    #'
    #' @return The current value of the \code{CMA} (invisibly)
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

#' Create a streamer for calculating the simple moving average
#'
#' @description \code{SMA} creates a streaming algorithm that can be used to
#' keep track of the mean of the previous k datapoints
#'
#'
#' @docType class
#'
#' @examples
#' mean <- SMA$new(c(1, 2, 3, 4, 5), window = 3)
#' mean$value()
#' #> [1] 4
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
SMA <- R6::R6Class("SMA", public = list(
    #' @description Creates a new \code{SMA} streamer object.
    #'
    #' @param x values to be used during initialisation (optional)
    #' @param window size of the window
    #'
    #' @examples
    #' mean <- SMA$new(window = 5)
    #'
    #' @return The new \code{SMA} (invisibly)
    initialize = function(x = NULL, window = NULL) {
        private$check_window_size(window)
        private$window = window
        if (!is.null(x)) {
            if (window >= length(x)) {
                private$values <- x
            }
            else {
                private$values <- x[seq(length(x) - window + 1, length(x))]
            }
        }
        invisible(self)
    },
    #' @description Resets the \code{SMA} streamer object.
    #'
    #' @param x values to be added to the stream
    #'
    #' @examples
    #' mean <- SMA$new(c(1, 2, 3), window = 3)
    #' mean$update(c(4, 5))
    #'
    #' @return The updated \code{SMA} (invisibly)
    update = function(x) {
        n <- length(x)
        if (n >= private$window) {
            private$values <- x[(length(x) - private$window + 1):length(x)]
        } else {
            m <- length(private$values)
            private$values <- c(
                private$values[(m - (private$window - n) + 1):m],
                x
            )
        }
        invisible(self)
    },
    #' @description Returns the current value of the average.
    #'
    #' If the number of values in the stream is less than the size of the window,
    #' the returned values is the mean of the entire stream of data.
    #'
    #' @examples
    #' mean <- SMA$new(c(1, 2, 3, 4, 5), window = 3)
    #' mean$value()
    #' #> [1] 4
    #'
    #' @return The current value of \code{SMA} (invisibly)
    value = function() {
        if (is.null(private$values)) {
            return(0)
        }
        sum(private$values) / length(private$values)
    }
), private = list(
    values = NULL,
    window = NULL,
    check_window_size = function(window) {
        if (is.null(window)) {
            stop("Size of the window must be specified")
        }
        if (round(window) != window | window <= 0) {
            stop("Size of the window must be an integer > 0")
        }
    }
)
)

#' Create a streamer for calculating the weighted moving average
#'
#' @description \code{WMA} creates a streaming algorithm that can be used to
#' keep track of the weighted mean of incoming values.
#'
#' In an n-day WMA the latest day has weight n, the second latest n-1, etc.,
#' down to one.
#'
#'
#' @docType class
#'
#' @examples
#' mean <- WMA$new(c(1, 2))
#' mean$update(c(3, 4))
#' mean$value()
#' #> [1] 7.5
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
WMA <- R6::R6Class("WMA", public = list(
    #' @description Creates a new \code{WMA} streamer object.
    #'
    #' @param x values to be used during initialisation (optional)
    #'
    #' @examples
    #' weighted_mean <- WMA$new()
    #'
    #' @return The new \code{WMA} (invisibly)
    initialize = function(x = NULL) {
        if (!is.null(x)) {
            private$count <- private$count + length(x)
            private$weighted_sum <- seq(1, private$count) %*% x
        }
        invisible(self)
    },
    #' @description Resets the \code{WMA} streamer object.
    #'
    #' @param x values to be added to the stream
    #'
    #' @examples
    #' weighted_mean <- WMA$new()
    #' weighted_mean$update(c(1, 2))
    #'
    #' @return The updated \code{WMA} (invisibly)
    update = function(x) {
        n <- private$count
        k <- length(x)
        private$weighted_sum <- private$weighted_sum + seq(n + 1, n + k) %*% x
        private$count <- n + k
        invisible(self)
    },
    #' @description Returns the current value of the weighted average.
    #'
    #' @examples
    #' weighted_mean <- WMA$new(c(1, 2, 3, 4))
    #' weighted_mean$value()
    #' #> [1] 7.5
    #'
    #' @return The current value of the \code{WMA} (invisibly)
    value = function() {
        if (private$count == 0L) {
            return(0)
        }
        as.numeric(private$weighted_sum / private$count)
    }
), private = list(
    weighted_sum = 0,
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
#' exp_mean$value()
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
        if (!(alpha >= 0 && alpha <= 1)) {
            stop("The smoothing factor must take values between 0 and 1.")
        }
        private$alpha <- alpha
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
    #' @return The current value of the \code{EMA} (invisibly)
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

#' Create a streamer for calculating the population and sample variance
#'
#' @description \code{Variance} creates a streaming algorithm that can be used to
#' calculate the population and sample variance of incoming values
#'
#'
#' @docType class
#'
#' @examples
#' variance <- Variance$new(c(1, 2))
#' variance$update(c(3, 4))
#' variance$value
#' #> [1] 1.25
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
Variance <- R6::R6Class("Variance", public = list(
    #' @description Creates a new \code{Variance} streamer object.
    #'
    #' @param x values to be used during initialization (optional)
    #'
    #'
    #' @examples
    #' variance <- Variance$new()
    #'
    #' @return The new \code{Variance} (invisibly)
    initialize = function(x = NULL) {
        if (!is.null(x)) {
            private$sum <- private$sum + sum(x)
            private$count <- private$count + length(x)
            mean <- private$sum / private$count
            private$variance <- 1/private$count * sum((x - mean)**2)
        }
        invisible(self)
    },
    #' @description Returns the current value of the Variance.
    #'
    #' @param sample for choosing between the population and sample variance.
    #'    If `TRUE` the sample variance is returned.
    #'    If `FALSE` the population variance is returned.
    #'
    #' @examples
    #' variance <- Variance$new(x = c(1,2,3,4))
    #' variance$value()
    #' #> [1] 1.25
    #' variance$value(sample = TRUE)
    #' #> [1] 1.666667
    #'
    #' @return The current value of the \code{Variance} (invisibly)
    value = function(sample = FALSE) {
        if (private$count == 0L) {
            return(0)
        }
        if (sample) {
            n <- private$count
            return(as.numeric(private$variance * n / (n - 1)))
        }
        as.numeric(private$variance)
    },
    #' @description Updates the Variance with a stream of new values
    #'
    #' @param x a vector of values to update the Variance
    #'
    #' @examples
    #' variance <- Variance$new(c(1,2))
    #' variance$update(c(3,4))
    #'
    #' @return The updated \code{Variance} (invisibly)
    update = function(x) {
        if (private$count == 0L) {
            # take first element of x as the starting value
            private$count <- 1
            private$sum <- x[1]
            x <- x[2:length(x)]
        }
        k <- length(x)
        n <- private$count + k
        counts <- seq(private$count + 1, private$count + k)
        sums <- cumsum(x) + private$sum
        means <- sums / counts
        numerators <- seq(n - k + 1, n)
        denominators <- n * seq(n - k, n - 1)
        coefs <- numerators / denominators

        private$variance <- (n - k)/n * private$variance + coefs %*% (x - means)**2
        private$sum <- private$sum + sum(x)
        private$count <- n

        invisible(self)
    }
), private = list(
    variance = 0,
    sum = 0,
    count = 0L
)
)

#' Create a streamer for producing a random sample from a population
#'
#' @description \code{ReservoirSampler} creates a streaming algorithm that can
#' be used to obtain a random sample from a population that is too large to
#' fit in memory. The samples can be made reproducible can be using
#' `set.seed(...)` before initialising the streamer.
#'
#' Implementation is based on doi:10.1145/198429.198435.
#'
#' @docType class
#'
#' @examples
#' sampler <- ReservoirSampler$new(k = 10)
#' for (i in 1:100) {
#'     sampler$update(i)
#' }
#' len(sampler$value())  # random sample from 1:100 of size 10
#' #> [1] 10
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
ReservoirSampler <- R6::R6Class("ReservoirSampler", public = list(
    #' @description Creates a new \code{ReservoirSampler} streamer object.
    #'
    #' @param k the desired sample size
    #'
    #' @return The new \code{ReservoirSampler} (invisibly)
    initialize = function(k) {
        private$k <- k
        private$w <- 1
        private$wait <- private$update_wait()
        invisible(self)
    },
    #' @description Update the \code{ReservoirSampler} streamer object.
    #'
    #' @param x values to be added to the stream
    #'
    #' @return The updated \code{ReservoirSampler} (invisibly)
    update = function(x) {
        private$update_values(x)
        invisible(self)
    },
    #' @description Returns the current random sample.
    #'
    #' @return The current sample of the \code{ReservoirSampler}
    value = function() {
        if (length(private$reservoir) < private$k) {
            stop("population must be at least size k")
        }
        private$reservoir
    }
), private = list(
    k = NULL,
    w = NULL,  # used to generate `wait` according to doi:10.1145/198429.198435
    wait = NULL,  # number of inputs to ignore before replacing reservoir value
    reservoir = c(),
    update_wait = function() {
        private$w <- private$w * exp(log(runif(1)) / private$k)
        floor(log(runif(1)) / log(1 - private$w)) + 1
    },
    update_values = function(x) {
        l <- length(private$reservoir)
        k <- private$k
        # Fill reservoir if smaller than k
        if (l < k) {
            private$reservoir <- append(private$reservoir,
                                        x[1:min(k - l, length(x))])
            # Continue recursively on remaining elements
            if (length(x) - (k - l) > 0) {
                private$update_values(x[(k - l + 1):length(x)])
            }
        } else {
            # Wait is longer than input
            if (private$wait > length(x)) {
                private$wait <- private$wait - length(x)
            # Replace random element in reservoir with input and reset wait
            } else {
                s <- private$wait
                private$reservoir[sample(1:k, 1)] <- x[s]
                private$wait <- private$update_wait()
                # Continue recursively on remaining elements
                if (length(x) - s > 0) {
                    private$update_values(x[(s + 1):length(x)])
                }
            }
        }
    }
)
)
