#' Create a streamer for updating a linear regression model
#'
#' @description \code{LinearRegressions} creates a streaming algorithm that can
#' calculate and update linear regression coefficients.
#'
#' @docType class
#'
#' @examples
#' X <- matrix(c(1, 2, 3, 5), ncol = 2)
#' y <- c(1, 3)
#' mean <- LinearRegression$new(X, y)
#' X_new <- matrix(c(5, 6), ncol = 2)
#' y_new <- 3
#' mean$update(X_new, y_new)
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
LinearRegression <- R6::R6Class("Mean", public = list(
    #' @description Creates a new \code{LinearRegression} streamer object.
    #'
    #' @param X initial design matrix
    #' @param y initial labels
    #'
    #' @return The new \code{LinearRegression} (invisibly)
    initialize = function(X, y) {
        # Validation
        if (!is.matrix(X)) stop("X must be a matrix")
        if (!is.vector(y)) stop("y must be a vector")
        # Don't need to store original y
        private$X <- X
        private$beta <- solve(t(X) %*% X, t(X) %*% y)
        invisible(self)
    },
    #' @description Updates the regression coefficient based on new data
    #'
    #' @param X new design matrix (currently must be 1xp)
    #' @param y new labels (currently must be of length 1)
    #'
    #'
    #' @return The updated \code{LinearRegression} (invisibly)
    update = function(X, y) {
        # TODO: write blog post showing the derivation of this
        # TODO: modify to allow for batch update
        # Validation
        if (!is.matrix(X)) stop("X must be a matrix")
        if (!is.vector(y)) stop("y must be a vector")
        if (!(ncol(X) == ncol(private$X))) stop(
            "new design matrix has an incorrect number of columns"
        )
        if (!(nrow(X) == 1 && length(y) == 1)) stop(
            "batch updates not implemented"
        )
        # Update coefficients
        C <- t(private$X) %*% private$X  # covariance matrix
        magnitude <- as.numeric((y - X %*% private$beta) / (1 + X %*% solve(C, t(X))))
        direction <- solve(C, t(X))
        private$beta <- private$beta + magnitude * direction
    },
    #' @description Returns the current coefficient vector.
    #'
    #' @return The current coefficient vector
    value = function() {
        private$beta
    }
), private = list(
    X = NULL,
    beta = NULL
)
)
