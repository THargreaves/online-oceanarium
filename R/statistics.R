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
        private$window <- window
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
    #' If the number of values in the stream is less than the size of
    #' the window, the returned values is the mean of the entire stream of data.
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
        private$weighted_sum <- valpha %*% x +
            (1 - private$alpha)**k * private$weighted_sum
        private$count <- private$count + k
        private$weighted_count <- (1 - (1 - private$alpha)**private$count)
        private$weighted_count <- private$weighted_count / private$alpha
    }
)
)

#' Create a streamer for calculating the population and sample variance
#'
#' @description \code{Variance} creates a streaming algorithm that can be
#' used to calculate the population and sample variance of incoming values
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
            private$variance <- 1 / private$count * sum((x - mean)**2)
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

        private$variance <- (n - k) / n * private$variance +
            coefs %*% (x - means)**2
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
#' length(sampler$value())  # random sample from 1:100 of size 10
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


#' Create a streamer for the modified secretary problem based on maximising the
#' expected score of a candidate when sampling from a known distribution
#' of scores.
#'
#' @description \code{SecretarySampler} creates a streamer object to reject
#'  or accept a candidate based on their score. The assumptions of the process
#'  are as follows:
#'  - we are allowed to make at most N successive draws from a hypothetical
#'  population of candidates with a known distribution function of scores
#'  - we are allowed to stop at the end of any draw and we gain the score of
#'  the currently observed candidate minus the total cost of observing
#'  all previous candidates
#'  - if we decide to continue sampling, it is not possible to go back to
#'  a previous candidate
#'  - if we decide to stop or reach the last candidate, the process ends.
#'
#' Implementation is based on doi:10.1016/0022-247X(61)90023-3
#'
#' @docType class
#'
#' @examples
#' set.seed(0)
#' candidate_scores <- rexp(10, rate = 1)
#' distr = list(func = "exp", rate = 1)
#' secretary <- SecretarySampler$new(10, c = 0, distr = distr)
#' i <- 1
#' while(secretary$value()$state == "CONTINUE"
#'     && i <= length(candidate_scores)) {
#'    secretary$update(candidate_scores[i])
#'    i <- i + 1
#' }
#' secretary$value()
#' @export
#' @format An \code{\link{R6Class}} generator object
SecretarySampler <- R6::R6Class("SecretarySampler", public = list(
    #' @description Creates a new \code{SecretarySampler} streamer object.
    #'
    #' @param N the maximum number of candidates to consider
    #' @param c the cost of observing one candidate
    #' @param distr list specifying the distribution of candidate scores
    #'
    #' @examples
    #' distr <- list(func = "exp", "rate" = 1)
    #' secretary <- SecretarySampler$new(N = 10, c = 0, distr = distr)
    #'
    #' @return The new \code{SecretarySampler} (invisibly)
    initialize = function(N, c = 0, distr) {
        if (c < 0) {
            stop("Observation cost must be non-negative")
        }
        if (!(N > 0 && N %% 1 == 0)) {
            stop("The total number of candidates must be a positive integer")
        }
        private$N <- N
        private$c <- c
        private$state <- "CONTINUE"
        private$distr <- distr
        private$critical_values <- private$find_critical_values(N, c, distr)
        private$n_observed <- 0
        if (private$critical_values[N] - c < 0) {
            private$state <- "STOP"
            stop("It is not optimal to start sampling. Check the value of c")
        }
        invisible(self)
    },
    #' @description Update the \code{SecretarySampler} streamer object.
    #'
    #' @param x a single observed score of a candidate
    #'
    #' @examples
    #' secretary$update(2.5)
    #'
    #' @return The updated \code{SecretarySampler} (invisibly)
    update = function(x) {
        if (length(x) > 1) {
            stop("Only single-value allowed in one step")
        }
        if (private$state == "STOP") {
            stop("Candidate already chosen")
        }
        private$n_observed <- private$n_observed + 1
        n_left <- private$N - private$n_observed
        # Accept if score greater than the critical value - cost
        # for the last candidate accept any non-negative score
        if ((private$n_observed == private$N) ||
            x > private$critical_values[n_left] - private$c) {
            private$state <- "STOP"
            if (x > 0) {
                private$optimal_score <- x
            }
        }
        invisible(self)
    },
    #' @description Returns the state of the \code{SecretarySampler}
    #'
    #' @examples
    #' secretary$value()$state
    #' #> [1] "STOP"
    #'
    #' @return list with summary of the state of the secretary object.
    value = function() {
        value <- list(
            state = private$state,
            score = private$optimal_score,
            n_observed = private$n_observed,
            total_cost = private$n_observed * private$c,
            critical_values = private$critical_values
        )
        return(value)
    }
), private = list(
    N = NULL,
    c = NULL,
    distr = c(),
    critical_values = c(),
    n_observed = NULL,
    state = NULL,
    optimal_score = NULL,
    # Calculates A(x) as in doi:10.1016/0022-247X(61)90023-3
    find_a = function(x, distr) {
        func <- distr["func"]
        # find the values of A(x)
        if (func == "norm") {
            a <- (dnorm(x) - x * (1 - pnorm(x)))
        } else if (func == "exp") {
            rate <-  as.numeric(distr["rate"])
            a <- (1 / rate) * exp(-x * rate)
        } else if (func == "pois") {
            rate <-  as.numeric(distr["rate"])
            a <- rate * private$pois_cdf(floor(x), rate)
                - x * private$pois_cdf(floor(x) + 1, rate)
        }
        return(a)
    },
    # Used to find the mean of a distribution
    find_mean = function(distr) {
        func <- distr["func"]
        if (func == "norm") {
            mean <- 0
        } else if (func == "exp") {
            mean <- 1 / as.numeric(distr["rate"])
        } else if (func == "pois") {
            mean <- as.numeric(distr["rate"])
        }
        return(mean)
    },
    # Used to find the values determining the optimal stopping criterion.
    find_critical_values = function(N, c, distr) {
        private$check_distr(distr)  # check the parameters of distribution
        mu <- c(private$find_mean(distr))  # initialize with the mean of distr
        for (i in seq(1, N - 1)) { # dynamically calculate the critical values
            mu[i + 1] <- private$find_a(mu[i] - c, distr) + mu[i] - c
        }
        return(mu)
    },
    # Performs validity checks for the parameters of the distribution
    check_distr = function(distr) {
        # check func
        func <- distr["func"]
        available_func <- c("norm", "exp", "pois")
        if (is.null(func)) {
            stop("distribution function \"func\" is missing, with no default")
        } else if (!(func %in% available_func)) {
            stop(paste("distribution function \"func\" must be in one of: ",
                     available_func))
        }
        #check params of dist
        if (func %in% c("exp", "pois")) {
            if (is.null(distr$rate)) {
                stop("parameter \"rate\" is missing, with no default")
            } else if (as.numeric(distr$rate) <= 0) {
                stop("rate must take positive values")
            }
        }
    },
    # Function the upper tail of poisson cdf
    pois_cdf = function(k, rate) {
        if (k == 0) {
            cdf <- 1
        } else {
            seq <- seq(0, k - 1)
            cdf <- 1 - sum(rate^seq / factorial(seq)) * exp(-rate)
        }
        return(cdf)
    }
)
)
