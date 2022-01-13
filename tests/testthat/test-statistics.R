# TODO: Split into multiple tests
test_that("CMA provides correct results", {
    # Empty streamer
    mean <- CMA$new()
    expect_identical(mean$value, 0)
    # Update empty streamer
    mean$update(c(1, 2, 3))
    expect_identical(mean$value, 2)
    # Initialised streamer
    mean <- CMA$new(c(1, 2))
    expect_identical(mean$value, 1.5)
    # Updating
    mean$update(3)
    expect_identical(mean$value, 2)
})

test_that("SMA provides correct results", {
    # Empty streamer
    mean <- SMA$new(window = 3)
    expect_identical(mean$value, 0)
    # Update empty streamer
    mean$update(c(1, 2, 3))
    expect_identical(mean$value, 2)
    # Short window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 2)
    expect_identical(mean$value, 2.5)
    # Same window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 3)
    expect_identical(mean$value, 2)
    # Long window initialisation
    mean <- SMA$new(c(1, 2, 3), window = 4)
    expect_identical(mean$value, 2)
    # Updating short
    mean$update(c(4, 5))
    expect_identical(mean$value, 3.5)
    # Updating same
    mean$update(c(1, 2, 3, 4))
    expect_identical(mean$value, 2.5)
    # Updating long
    mean$update(c(1, 2, 3, 4, 5))
    expect_identical(mean$value, 3.5)
})

test_that("WMA provides correct results", {
    # Empty streamer
    weighted_mean <- WMA$new()
    expect_identical(weighted_mean$value, 0)
    # Update empty streamer
    weighted_mean$update(c(1, 2, 3, 4))
    expect_identical(weighted_mean$value, 7.5)
    # Initialised streamer
    weighted_mean <- WMA$new(c(1, 2, 3, 4))
    expect_identical(weighted_mean$value, 7.5)
    # Updating
    weighted_mean$update(10)
    expect_identical(weighted_mean$value, 16)
})

test_that("EMA provides correct results", {
    # Empty streamer
    exp_mean <- EMA$new()
    expect_identical(exp_mean$value, 0)
    # Update empty streamer
    exp_mean <- exp_mean$update(c(1, 2, 3))
    expect_equal(exp_mean$value, 2.428571, tolerance = 10e-6)
    # Initialised streamer with default alpha = 0.5
    exp_mean <- EMA$new(c(1, 2, 3))
    expect_equal(exp_mean$value, 2.428571, tolerance = 10e-6)
    # Initialised streamer with alpha = 0.8
    exp_mean <- EMA$new(c(1, 2, 3), alpha = 0.8)
    expect_equal(exp_mean$value, 2.774194, tolerance = 10e-6)
    # Updating
    exp_mean$update(4)
    expect_equal(exp_mean$value, 3.75641, tolerance = 10e-6)
})

test_that("Variance provides correct results", {
    ## Sample variance
    # Empty streamer
    variance <- Variance$new(sample = TRUE)
    expect_identical(variance$value, 0)
    # Update empty streamer
    variance$update(c(1, 2, 3, 4))
    expect_equal(variance$value, 1.666667, tolerance = 10e-6)
    # Initialised streamer
    variance <- Variance$new(c(1, 2, 3, 4), sample = TRUE)
    expect_equal(variance$value, 1.666667, tolerance = 10e-6)
    # Updating
    variance$update(5)
    expect_identical(variance$value, 2.5)

    ## Population variance
    # Empty streamer
    variance <- Variance$new()
    expect_identical(variance$value, 0)
    # Update empty streamer
    variance$update(c(1, 2, 3, 4))
    expect_identical(variance$value, 1.25)
    # Initialised streamer
    variance <- Variance$new(c(1, 2, 3, 4))
    expect_identical(variance$value, 1.25)
    # Updating
    variance$update(5)
    expect_identical(variance$value, 2)
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
        samples <- append(samples, paste(sort(sampler$value),
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
    expect_error(sampler$value)
})

test_that("SecretarySampler is correctly initialised", {
    distr <- list(func = "norm")
    N <- 10
    secretary <- SecretarySampler$new(N = N, c = 0.1, distr = distr)
    expect_null(secretary$value$score)
    expect_equal(secretary$value$state, "CONTINUE")
})

test_that("SecretarySampler is not initialised for cost too big", {
    distr <- list(func = "norm")
    N <- 10
    expect_error(SecretarySampler$new(N = N, c = 1, distr = distr))
})

test_that("SecretarySampler produces correct critical values for normal dist", {
    distr <- list(func = "norm")
    N <- 10
    secretary <- SecretarySampler$new(N = N, c = 0, distr = distr)
    expected_cv <- c(0.000, 0.399, 0.630, 0.790, 0.913, 1.011,
                     1.092, 1.162, 1.223, 1.276)
    cv <- round(secretary$value$critical_values, 3)
    expect_equal(expected_cv, cv, tolerance = 10e-3)
})

test_that("SecretarySampler produces correct critical values for exp dist", {
    distr <- list(func = "exp", rate = 1)
    N <- 10
    secretary <- SecretarySampler$new(N = N, c = 0, distr = distr)
    expected_cv <- c(1.000, 1.368, 1.623, 1.820, 1.982, 2.120,
                     2.240, 2.346, 2.442, 2.529)
    cv <- round(secretary$value$critical_values, 3)
    expect_equal(expected_cv, cv, tolerance = 10e-3)
})

test_that("SecretarySampler produces correct critical values for pois dist", {
    distr <- list(func = "pois", rate = 1)
    N <- 10
    secretary <- SecretarySampler$new(N = N, c = 0, distr = distr)
    expected_cv <- c(1.000, 1.632, 2.264, 2.528, 2.793, 3.057,
                     3.137, 3.218, 3.298, 3.378)
    cv <- round(secretary$value$critical_values, 3)
    expect_equal(expected_cv, cv, tolerance = 10e-3)
})

test_that("SecretarySampler passes through all scores without choosing
          the last candidate with negative score", {
    distr <- list(func = "norm")
    scores <- runif(10, -1, 0)
    secretary <- SecretarySampler$new(N = length(scores), c = 0, distr = distr)
    i <- 1
    while (secretary$value$state == "CONTINUE"
          && i <= length(scores)) {
        secretary$update(scores[i])
        i <- i + 1
    }
    expect_null(secretary$value$score)
    expect_equal(secretary$value$state, "STOP")
    expect_equal(secretary$value$n_observed, length(scores))
})

test_that("SecretarySampler passes through all scores and is forced to
          choose the last candidate.", {
    distr <- list(func = "norm")
    scores <- runif(10, -1, 0)
    scores <- c(scores, 0.1)
    secretary <- SecretarySampler$new(N = length(scores), c = 0, distr = distr)
    i <- 1
    while (secretary$value$state == "CONTINUE"
         || i <= length(scores)) {
      secretary$update(scores[i])
      i <- i + 1
    }
    expect_equal(secretary$value$score, 0.1)
    expect_equal(secretary$value$state, "STOP")
    expect_equal(secretary$value$n_observed, length(scores))
})

test_that("SecretarySampler correclty handles length and cost errors", {
    # zero length
    expect_error(SecretarySampler$new(N = 0, c = 0,
                                      distr = list()))
    # negative cost
    expect_error(SecretarySampler$new(N = 0, c = -2,
                                      distr = list()))
})

test_that("SecretarySampler handles distribution mispecifications", {
    # func not specified
    expect_error(SecretarySampler$new(N = 1, c = 0,
                                      distr = list()))
    # wrong type of dist
    expect_error(SecretarySampler$new(N = 1, c = 0,
                                      distr = list(func = "binom")))
    # rate not specified
    expect_error(SecretarySampler$new(N = 1, c = 0,
                                      distr = list(func = "exp")))
    # rate is negative
    expect_error(SecretarySampler$new(N = 1, c = 0,
                                      distr = list(func = "exp", rate = -2)))
})

test_that("SecretarySampler does not update if candidate already chosen", {
    distr <- list(func = "norm")
    secretary <- SecretarySampler$new(N = 10, c = 0, distr = distr)
    secretary$update(5)  # candidate should be chosen
    expect_error(secretary$update(4))  # no update
    expect_equal(secretary$value$state, "STOP")
    expect_equal(secretary$value$score, 5)
    expect_equal(secretary$value$n_observed, 1)
})


test_that("SecretarySampler update accepts only sinlge values", {
    distr <- list(func = "norm")
    secretary <- SecretarySampler$new(N = 10, c = 0, distr = distr)
    expect_error(secretary$update(c(1, 2, 3)))  # candidate should be chosen
})
