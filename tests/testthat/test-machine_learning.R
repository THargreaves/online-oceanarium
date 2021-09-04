test_that("LinearRegression provides correct results", {
    methods <- c(
        "sherman-morrison",
        "woodbury"
    )
    for (method in methods) {
        X <- matrix(c(3, 1, 4, 1, 5, 9, 2, 6), ncol = 2)
        y <- c(2, 7, 1, 8)
        # Initialisation
        reg <- LinearRegression$new(X, y, method = method)
        expect_equal(reg$value(), unname(coef(lm(y ~ X))))
        # Updating
        X_new <- matrix(c(5, 3, 5, 8), ncol = 2)
        y_new <- c(2, 8)
        reg$update(X_new, y_new)
        expect_equal(
            reg$value(),
            unname(coef(lm(c(y, y_new) ~ rbind(X, X_new))))
        )
        # No intercept
        reg <- LinearRegression$new(X, y, fit_intercept = FALSE,
                                    method = method)
        expect_equal(reg$value(), unname(coef(lm(y ~ X + 0))))
    }
})
