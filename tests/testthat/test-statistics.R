# TODO: Split into multiple tests
test_that("CMA provides correct results", {
    # Empty streamer
    mean <- CMA$new()
    expect_identical(mean$value(), 0)
    # Initialised streamer
    mean <- CMA$new(c(1, 2))
    expect_identical(mean$value(), 1.5)
    # Updating
    mean$update(3)
    expect_identical(mean$value(), 2)
})

test_that("SMA provides correct results", {
    # Empty streamer
    mean <- SMA$new(window = 3)
    expect_identical(mean$value(), 0)
    # Short window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 2)
    expect_identical(mean$value(), 2.5)
    # Same window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 3)
    expect_identical(mean$value(), 2)
    # Long window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 4)
    # TODO: See issue #8
    # expect_identical(mean$value(), 2)
    # Updating short
    mean$update(c(4, 5))
    expect_identical(mean$value(), 3.5)
    # Updating same
    mean$update(c(1, 2, 3, 4))
    expect_identical(mean$value(), 2.5)
    # Updating long
    mean$update(c(1, 2, 3, 4, 5))
    expect_identical(mean$value(), 3.5)
})
