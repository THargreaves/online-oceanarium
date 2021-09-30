# TODO: Split into multiple tests
test_that("CMA provides correct results", {
    # Empty streamer
    mean <- CMA$new()
    expect_identical(mean$value(), 0)
    # Update empty streamer
    mean$update(c(1, 2, 3))
    expect_identical(mean$value(), 2)
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
    # Update empty streamer
    mean$update(c(1, 2, 3))
    expect_identical(mean$value(), 2)
    # Short window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 2)
    expect_identical(mean$value(), 2.5)
    # Same window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 3)
    expect_identical(mean$value(), 2)
    # Long window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 4)
    expect_identical(mean$value(), 2)
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

test_that("WMA provides correct results", {
    # Empty streamer
    weighted_mean <- WMA$new()
    expect_identical(weighted_mean$value(), 0)
    # Update empty streamer
    weighted_mean$update(c(1, 2, 3, 4))
    expect_identical(weighted_mean$value(), 7.5)
    # Initialised streamer
    weighted_mean <- WMA$new(c(1, 2, 3, 4))
    expect_identical(weighted_mean$value(), 7.5)
    # Updating
    weighted_mean$update(10)
    expect_identical(weighted_mean$value(), 16)
})

test_that("EMA provides correct results", {
    # Empty streamer
    exp_mean <- EMA$new()
    expect_identical(exp_mean$value(), 0)
    # Update empty streamer
    exp_mean <- exp_mean$update(c(1, 2, 3))
    expect_equal(exp_mean$value(), 2.428571, tolerance = 10e-6)
    # Initialised streamer with default alpha = 0.5
    exp_mean <- EMA$new(c(1, 2, 3))
    expect_equal(exp_mean$value(), 2.428571, tolerance = 10e-6)
    # Initialised streamer with alpha = 0.8
    exp_mean <- EMA$new(c(1, 2, 3), alpha = 0.8)
    expect_equal(exp_mean$value(), 2.774194, tolerance = 10e-6)
    # Updating
    exp_mean$update(4)
    expect_equal(exp_mean$value(), 3.75641, tolerance = 10e-6)
})

test_that("Variance provides correct results", {
    # Empty streamer
    variance <- Variance$new()
    expect_identical(variance$value(), 0)
    # Update empty streamer
    variance$update(c(1, 2, 3, 4))
    expect_identical(variance$value(), 1.25)
    expect_equal(variance$value(sample = TRUE), 1.666667, tolerance = 10e-6)
    # Initialised streamer
    variance <- Variance$new(c(1, 2, 3, 4))
    expect_identical(variance$value(), 1.25)
    expect_equal(variance$value(sample = TRUE), 1.666667, tolerance = 10e-6)
    # Updating
    variance$update(5)
    expect_identical(variance$value(), 2)
    expect_identical(variance$value(sample = TRUE), 2.5)
})

test_that("ReservoirSampler produces valid samples", {
    # Sampling 2 elements from 4
    # Each sample should appear roughly 1/6 of the time
    samples <- c()
    for (i in 1:2000) {
        set.seed(i)
        sampler <- ReservoirSampler$new(k = 2)
        for (x in 1:4) {
            sampler$update(x)
        }
        samples <- append(samples, paste(sort(sampler$value()),
                                         collapse = ""))
    }
    props <- prop.table(table(samples))
    expect_equal(as.numeric(props), rep(1 / 6, 6), tolerance = 10e-2)
    # Add more elements than sample size
    sampler <- ReservoirSampler$new(k = 2)
    sampler$update(1:3)
    # Add more elements than wait time
    set.seed(1729)
    sampler <- ReservoirSampler$new(k = 1)
    sampler$update(1)
    sampler$update(2:3)
})

test_that("ReservoirSampler handles errors", {
    # Requesting sample before supplying enough elements
    sampler <- ReservoirSampler$new(k = 2)
    sampler$update(1)
    expect_error(sampler$value())
})
