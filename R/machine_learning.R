#' Create a streamer for updating a linear regression model
#'
#' @description \code{LinearRegressions} creates a streaming algorithm that can
#' calculate and update linear regression coefficients.
#'
#' @docType class
#'
#' @examples
#' X <- matrix(c(3, 1, 4, 1, 5, 9, 2, 6), ncol = 2)
#' y <- c(2, 7, 1, 8)
#' reg <- LinearRegression$new(X, y)
#' X_new <- matrix(c(5, 3, 5, 8), ncol = 2)
#' y_new <- c(2, 8)
#' reg$update(X_new, y_new)
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
LinearRegression <- R6::R6Class("LinearRegression", public = list(
    #' @description Creates a new \code{LinearRegression} streamer object.
    #'
    #' @param X initial design matrix
    #' @param y initial labels
    #' @param fit_intercept if `TRUE`, add an intercept to the model
    #' @param method the method used to update regression coefficients
    #'
    #' @return The new \code{LinearRegression} (invisibly)
    initialize = function(X, y, fit_intercept = TRUE,
                          method = "woodbury") {
        # Validation
        private$validate_data(X, y)
        # TODO: how to validate Booleans?
        if (!method %in% private$valid_methods) {
            stop(paste(
                "method must be one of",
                paste(private$valid_methods, collaps = ", ")
            ))
        }
        private$method <- method
        # Add intercept
        private$fit_intercept <- fit_intercept
        if (private$fit_intercept) {
            X <- cbind(rep(1, nrow(X)), X)
        }
        # Dependency on y captured through beta so don't need to store
        private$X <- X
        private$C <- t(private$X) %*% private$X
        # TODO: create custom solve method with error checking and conversion
        private$beta <- as.numeric(solve(private$C, t(private$X) %*% y))
        invisible(self)
    },
    #' @description Updates the regression coefficient based on new data
    #'
    #' @param X new design matrix
    #' @param y new labels
    #'
    #' #' @return The updated \code{LinearRegression} (invisibly)
    update = function(X, y) {
        # Validation
        private$validate_data(X, y)
        # Add intercept
        if (private$fit_intercept) {
            X <- cbind(rep(1, nrow(X)), X)
        }
        # Validation (cont.)
        if (!(ncol(X) == ncol(private$X))) stop(paste(
            "new design matrix must have the same number of predictors as the",
            "current", sprintf("(%d != %d)", ncol(X), ncol(private$X))
        ))
        # Dispatch update method (ordered by preference)
        if (private$method == "woodbury") {
            private$woodbury_update(X, y)
        } else if (private$method == "sherman-morrison") {
            private$sherman_morrison_update(X, y)
        }
    },
    #' @description Returns the current coefficient vector.
    #'
    #' @return The current coefficient vector
    value = function() {
        private$beta
    }
), private = list(
    X = NULL,
    C = NULL,  # covariance matrix
    beta = NULL,  # coefficients
    fit_intercept = NULL,
    method = NULL,
    valid_methods = c(
        "sherman-morrison",
        "woodbury"
    ),
    validate_data = function(X, y) {
        if (!is.matrix(X)) stop("X must be a matrix")
        if (!is.vector(y)) stop("y must be a vector")
        if (!nrow(X) == length(y)) {
            stop(paste(
                "X and y must have the same number of observations",
                sprintf("(%d != %d)", nrow(X), length(y))
            ))
        }
    },
    sherman_morrison_update = function(X, y) {
        # Repeated apply update
        for (i in seq_len(nrow(X))) {
            Xi <- X[i, ]  # becomes a column vector
            yi <- y[i]
            # Update coefficients
            magnitude <- (
                (yi - sum(Xi * private$beta)) /
                (1 + sum(Xi * solve(private$C, Xi)))
            )
            direction <- as.numeric(solve(private$C, Xi))  # 1xp mtrx -> vector
            private$beta <- private$beta + magnitude * direction
            # Update covariance matrix
            private$C <- private$C + outer(Xi, Xi)
        }
    },
    woodbury_update = function(X, y) {
        # Update coefficients
        n <- nrow(X)
        inverse_term <- diag(n) + X %*% solve(private$C, t(X))
        update <- as.numeric(
            t(solve(inverse_term, t(solve(private$C, t(X))))) %*%
            (y - X %*% private$beta)
        )  # 1xp mtrx -> vector
        private$beta <- private$beta + update
        # Update covariance matrix
        private$C <- private$C + t(X) %*% X
    }
)
)
