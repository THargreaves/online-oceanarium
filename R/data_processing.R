ArraySorting <- R6::R6Class("ArraySorting", public = list(
    initialize = function(x = NULL) {
        # Reset if already contains values
        private$sorted_values <- c()
        if (!is.null(x)) {
            private$insert_values(x)
        }
        invisible(self)
    },
    update = function(x) {
        private$insert_values(x)
        invisible(self)
    },
    value = function() {
        private$sorted_values
    }
), private = list(
    sorted_values = c(),
    insert_values = function(x) {
        for (e in x) {
            # If array is empty insert as only element
            if (length(private$sorted_values) == 0) {
                private$sorted_values <- e
            } else {
                # Scan through the array and insert before the first value
                # that is larger than it
                for (i in seq_along(private$sorted_values)) {
                    inserted <- FALSE
                    if (e <= private$sorted_values[i]) {
                        private$sorted_values <- append(
                            private$sorted_values, e, after = i - 1
                        )
                        inserted <- TRUE
                        break
                    }
                }
                # If not inserted, it is larger than all values, so add to end
                if (!inserted) {
                    private$sorted_values <- c(private$sorted_values, e)
                }
            }
        }
    }
)
)

Node <- R6::R6Class("Node", public = list(
    initialize = function(data, link = NULL) {
        self$data <- data
        self$link <- link
    },
    data = NULL,
    link = NULL
))

LinkedListSorting <- R6::R6Class("LinkedListSorting", public = list(
    initialize = function(x = NULL) {
        # Reset if already contains values
        private$head <- NULL
        if (!is.null(x)) {
            private$insert_values(x)
        }
        invisible(self)
    },
    update = function(x) {
        private$insert_values(x)
        invisible(self)
    },
    value = function() {
        value <- c()
        if (is.null(private$head)) {
            return(value)
        }
        curr <- private$head
        repeat {
            count <- curr$data$count
            value <- append(value, rep(curr$data$value, count))
            curr <- curr$link
            if (is.null(curr)) {
                break
            }
        }
        value
    }
), private = list(
    head = NULL,
    insert_values = function(x) {
        for (e in x) {
            # Linked list is empty
            if (is.null(private$head)) {
                private$head <- Node$new(list(
                    value = e,
                    count = 1
                ), NULL)
            # New value proceeds linked list
            } else if (e < private$head$data$value) {
                private$head <- private$head <- Node$new(data = list(
                    value = e,
                    count = 1
                ), link = private$head)
            }
            else {
                # Traverse linked list to see where to insert
                last <- NULL
                curr <- private$head
                repeat {
                    # Insert before curr item
                    if (e < curr$data$value) {
                        # NB: last won't be NULL since we check e < start above
                        new_node <- Node$new(data = list(
                            value = e,
                            count = 1
                        ), link = curr)
                        last$link <- new_node
                        break
                    # Item already in linked list
                    } else if (e == curr$data$value) {
                        curr$data$count <- curr$data$count + 1
                        break
                    # Reached end of list or move to next item
                    } else {
                        last <- curr
                        curr <- curr$link
                        if (is.null(curr)) {
                            new_node <- Node$new(data = list(
                                value = e,
                                count = 1
                            ), link = NULL)
                            last$link <- new_node
                            break
                        }
                    }
                }
            }
        }
    }
)
)

#' Create a streamer for sorting incoming data
#'
#' @description \code{Sorting} creates a streaming algorithm that can be used
#' to sort incoming data
#'
#' @docType class
#'
#' @examples
#' sorter <- Sorting$new(c(3, 1))
#' sorter$update(c(2, 4))
#' sorter$value()
#' #> [1] 1 2 3 4
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
# TODO: benchmark this against simple insertion
Sorting <- R6::R6Class("Sorting", public = list(
    #' @description Creates a new \code{Sorting} streamer object.
    #'
    #' @param x values to be used during initialisation (optional)
    #' @param method the method used to sort values
    #'
    #' @return The new \code{Sorting} (invisibly)
    initialize = function(x = NULL, method = "linked-list") {
        methods <- names(private$method_lookup)
        if (!method %in% methods) {
            stop(paste(
                "method must be one of",
                paste(methods, collaps = ", ")
            ))
        }
        private$sorter <- private$method_lookup[[method]]$new(x)
        invisible(self)
    },
    #' @description Updates the \code{Sorting} streamer object.
    #'
    #' @param x values to be added to the stream
    #'
    #' @return The updated \code{Sorting} (invisibly)
    update = function(x) {
        private$sorter$update(x)
        invisible(self)
    },
    #' @description Returns the current value of the \code{Sorting}.
    #'
    #' @return The current value of the \code{Sorting}
    value = function() {
        private$sorter$value()
    }
), private = list(
    method_lookup = list(
        `array` = ArraySorting,
        `linked-list` = LinkedListSorting
    ),
    sorter = NULL
)
)
