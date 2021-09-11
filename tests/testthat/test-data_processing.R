test_that("Linked list sorting provides correct results", {
    method <- "linked-list"
    # Empty streamer
    sorter <- Sorting$new(method = method)
    expect_identical(sorter$value(), c())
    # Initialised streamer
    sorter <- Sorting$new(c(3, 2, 5), method = method)
    expect_identical(sorter$value(), c(2, 3, 5))
    # Insert before
    sorter$update(1)
    expect_identical(sorter$value(), c(1, 2, 3, 5))
    # Insert into
    sorter$update(4)
    expect_identical(sorter$value(), c(1, 2, 3, 4, 5))
    # Insert after
    sorter$update(6)
    expect_identical(sorter$value(), c(1, 2, 3, 4, 5, 6))
    # Insert again
    sorter$update(3)
    expect_identical(sorter$value(), c(1, 2, 3, 3, 4, 5, 6))
})

test_that("Array sorting provides correct results", {
    method <- "array"
    # Empty streamer
    sorter <- Sorting$new(method = method)
    expect_identical(sorter$value(), c())
    # Initialised streamer
    sorter <- Sorting$new(c(3, 2, 5), method = method)
    expect_identical(sorter$value(), c(2, 3, 5))
    # Insert before
    sorter$update(1)
    expect_identical(sorter$value(), c(1, 2, 3, 5))
    # Insert into
    sorter$update(4)
    expect_identical(sorter$value(), c(1, 2, 3, 4, 5))
    # Insert after
    sorter$update(6)
    expect_identical(sorter$value(), c(1, 2, 3, 4, 5, 6))
    # Insert again
    sorter$update(3)
    expect_identical(sorter$value(), c(1, 2, 3, 3, 4, 5, 6))
})
