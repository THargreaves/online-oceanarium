LinkedListSorting <- R6::R6Class("LinkedListSorting", public = list(
    initialize = function(x = NULL) {
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
        if (is.null(private$start)) {
            return(value)
        }
        curr <- private$start
        repeat {
            count <- private$linked_list[[curr]]$count
            value <- append(value, rep(curr, count))
            curr <- private$linked_list[[curr]]$link
            if (is.null(curr)) {
                break
            }
        }
        value
    }
), private = list(
    start = NULL,
    linked_list = list(),  # key -> (count, link)
    insert_values = function(x) {
        for (e in x) {
            if (is.null(private$start)) {  # linked list is empty
                private$linked_list[[e]] <- list(count = 1, link = NULL)
                private$start <- e
            } else if (e < private$start) {  # new value proceeds linked list
                private$linked_list[[e]] <-
                    list(count = 1, link = private$start)
                private$start <- e
            }
            else {
                # Traverse linked list to see where to insert
                last <- NULL
                curr <- private$start
                repeat {
                    if (e < curr) {  # insert before curr item
                        # NB: last won't be NULL since we check e < start above
                        private$linked_list[[last]]$link <- e
                        private$linked_list[[e]] <- list(count = 1, link = curr)
                        break
                    } else if (e == curr) {  # item already in linked list
                        private$linked_list[[e]]$count <-
                            private$linked_list[[e]]$count + 1
                        break
                    } else {  # reached end of list or move to next item
                        last <- curr
                        curr <- private$linked_list[[curr]]$link
                        if (is.null(curr)) {
                            private$linked_list[[e]] <-
                                list(count = 1, link = NULL)
                            private$linked_list[[last]]$link <- e
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
        `linked-list` = LinkedListSorting,
        `array` = NULL
    ),
    sorter = NULL
)
)
