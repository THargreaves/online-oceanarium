#' Create a streamer for calculating a running mean
#'
#' @description \code{Mean} creates a streaming algorithm that can be used to
#' keep track of the mean of incoming values.
#'
#' @details Assemble \code{Node} objects into a \code{data.tree}
#' structure and use the traversal methods to set, get, and perform operations on it. Typically, you construct larger tree
#' structures by converting from \code{data.frame}, \code{list}, or other formats.
#'
#' Most methods (e.g. \code{node$Sort()}) also have a functional form (e.g. \code{Sort(node)})
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
    #' mean$reset()
    #'
    #'
    #' @return The new \code{Mean} (invisibly)
    initialize = function(x = NULL) {
        if (!is.null(x)) {
            self$sum <- self$sum + sum(x)
            self$count <- self$count + length(x)
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
        self$sum <- self$sum + sum(x)
        self$count <- self$count + length(x)
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
        if (self$count == 0L) {
            return(0)
        }
        self$sum / self$count
    },
    #' @description Resets the \code{Mean} streamer object.
    #'
    #' @param x values to be used during re-initialisation (optional)
    #'
    #' @examples
    #' mean <- Mean$new()
    #' mean$reset()
    #'
    #' @return The updated \code{Mean} (invisibly)
    reset = function(x = NULL) {
        if (!is.null(x)) {
            self$sum <- sum(x)
            self$count <- length(x)
        } else {
            self$sum <- 0
            self$count <- 0L
        }
        invisible(self)
    }
    ), private = list(
        sum = 0,
        count = 0L
    )
)
