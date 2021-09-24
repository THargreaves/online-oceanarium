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
})
