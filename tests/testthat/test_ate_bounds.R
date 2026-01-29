context("ate_bounds")

test_that("ate_bounds returns correct structure", {
  skip_if_not_installed("np")

  set.seed(42)
  N <- 50
  x <- rnorm(N)
  d <- rbinom(N, 1, pnorm(0.5 * x))
  y0 <- rbinom(N, 1, pnorm(-0.3 + 0.2 * x))
  y1 <- rbinom(N, 1, pnorm(0.3 + 0.2 * x))
  y <- d * y1 + (1 - d) * y0
  df <- data.frame(y = y, x = x, d = d)

  res <- ate_bounds(y ~ x, treatment = d, data = df)

  expect_s3_class(res, "rbounds_ate")
  expect_true("ate_lower" %in% names(res))
  expect_true("ate_upper" %in% names(res))
  expect_true("ate_lower_ci" %in% names(res))
  expect_true("ate_upper_ci" %in% names(res))
  expect_true("cate_lower" %in% names(res))
  expect_true("cate_upper" %in% names(res))
  expect_length(res$cate_lower, N)
  expect_length(res$propensity, N)
})

test_that("ate_bounds bounds are ordered correctly", {
  skip_if_not_installed("np")

  set.seed(123)
  N <- 50
  x <- rnorm(N)
  d <- rbinom(N, 1, pnorm(0.5 * x))
  y0 <- rbinom(N, 1, pnorm(-0.3 + 0.2 * x))
  y1 <- rbinom(N, 1, pnorm(0.3 + 0.2 * x))
  y <- d * y1 + (1 - d) * y0
  df <- data.frame(y = y, x = x, d = d)

  res <- ate_bounds(y ~ x, treatment = d, data = df)

  # Lower CI <= Lower bound <= Upper bound <= Upper CI
  expect_true(res$ate_lower_ci <= res$ate_lower + 1e-10)
  expect_true(res$ate_lower <= res$ate_upper + 1e-10)
  expect_true(res$ate_upper <= res$ate_upper_ci + 1e-10)

  # CATE bounds should respect the same ordering
  expect_true(all(res$cate_lower <= res$cate_upper + 1e-10))

  # ATE bounds for binary outcomes should be in [-1, 1]
  expect_true(res$ate_lower >= -1 - 1e-10)
  expect_true(res$ate_upper <= 1 + 1e-10)
})

test_that("ate_bounds with custom y_min and y_max", {
  skip_if_not_installed("np")

  set.seed(42)
  N <- 50
  x <- rnorm(N)
  d <- rbinom(N, 1, 0.5)
  y <- d * runif(N, 0.2, 0.8) + (1 - d) * runif(N, 0.1, 0.6)
  df <- data.frame(y = y, x = x, d = d)

  res <- ate_bounds(y ~ x, treatment = d, data = df, y_min = 0, y_max = 1)

  expect_s3_class(res, "rbounds_ate")
  expect_equal(res$y_min, 0)
  expect_equal(res$y_max, 1)
})

test_that("ate_bounds errors on non-binary treatment", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  d <- rnorm(N)  # continuous, not binary
  y <- rbinom(N, 1, 0.5)
  df <- data.frame(y = y, x = x, d = d)

  expect_error(ate_bounds(y ~ x, treatment = d, data = df), "binary")
})

test_that("ate_bounds errors when treatment not in data", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  y <- rbinom(N, 1, 0.5)
  df <- data.frame(y = y, x = x)
  d <- rbinom(N, 1, 0.5)  # d not in df

  expect_error(ate_bounds(y ~ x, treatment = d, data = df), "must appear in data")
})

test_that("ate_bounds errors when all treated or all control", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  y <- rbinom(N, 1, 0.5)

  # All treated
  df1 <- data.frame(y = y, x = x, d = rep(1, N))
  expect_error(ate_bounds(y ~ x, treatment = d, data = df1), "Both treatment and control")

  # All control
  df2 <- data.frame(y = y, x = x, d = rep(0, N))
  expect_error(ate_bounds(y ~ x, treatment = d, data = df2), "Both treatment and control")
})

test_that("ate_bounds errors when outcome out of bounds", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  d <- rbinom(N, 1, 0.5)
  y <- rnorm(N)  # can be outside [0, 1]
  df <- data.frame(y = y, x = x, d = d)

  expect_error(ate_bounds(y ~ x, treatment = d, data = df, y_min = 0, y_max = 1),
               "must be in")
})

test_that("print.rbounds_ate works", {
  skip_if_not_installed("np")

  set.seed(42)
  N <- 50
  x <- rnorm(N)
  d <- rbinom(N, 1, 0.5)
  y <- rbinom(N, 1, pnorm(0.3 * d + 0.2 * x))
  df <- data.frame(y = y, x = x, d = d)

  res <- ate_bounds(y ~ x, treatment = d, data = df)

  expect_output(ret <- print(res), "Manski Bounds")
  expect_identical(ret, res)
})

test_that("plot.rbounds_ate works", {
  skip_if_not_installed("np")
  skip_if_not_installed("ggplot2")

  set.seed(42)
  N <- 50
  x <- rnorm(N)
  d <- rbinom(N, 1, 0.5)
  y <- rbinom(N, 1, pnorm(0.3 * d + 0.2 * x))
  df <- data.frame(y = y, x = x, d = d)

  res <- ate_bounds(y ~ x, treatment = d, data = df)

  p1 <- plot(res, type = "cate")
  expect_s3_class(p1, "ggplot")

  p2 <- plot(res, type = "potential")
  expect_s3_class(p2, "ggplot")
})
