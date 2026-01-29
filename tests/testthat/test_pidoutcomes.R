context("pidoutcomes")

test_that("pidoutcomes returns correct structure", {
  skip_if_not_installed("np")

  set.seed(42)
  N <- 50
  x <- rnorm(N)
  z <- rbinom(N, 1, 0.7)
  y <- rbinom(N, 1, pnorm(x))
  df <- data.frame(y = y, x = x, z = z)

  res <- pidoutcomes(y ~ x, z, df)

  expect_s3_class(res, "rbounds")
  expect_named(res, c("lower_ci", "lower", "upper", "upper_ci"))
  expect_length(res$lower, N)
  expect_length(res$upper, N)
  expect_length(res$lower_ci, N)
  expect_length(res$upper_ci, N)
})

test_that("pidoutcomes bounds are ordered correctly", {
  skip_if_not_installed("np")

  set.seed(123)
  N <- 50
  x <- rnorm(N)
  z <- rbinom(N, 1, 0.7)
  y <- rbinom(N, 1, pnorm(x))
  df <- data.frame(y = y, x = x, z = z)

  res <- pidoutcomes(y ~ x, z, df)

  # Lower CI <= Lower bound <= Upper bound <= Upper CI
  expect_true(all(res$lower_ci <= res$lower + 1e-10))
  expect_true(all(res$lower <= res$upper + 1e-10))
  expect_true(all(res$upper <= res$upper_ci + 1e-10))

  # Bounds should be in [0, 1] for binary outcome
  expect_true(all(res$lower >= -1e-10))
  expect_true(all(res$upper <= 1 + 1e-10))
})

test_that("pidoutcomes errors on non-binary outcome", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  z <- rbinom(N, 1, 0.7)
  y <- rnorm(N)  # continuous, not binary
  df <- data.frame(y = y, x = x, z = z)

  expect_error(pidoutcomes(y ~ x, z, df), "binary")
})

test_that("pidoutcomes errors on non-binary z", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  z <- rnorm(N)  # continuous, not binary
  y <- rbinom(N, 1, 0.5)
  df <- data.frame(y = y, x = x, z = z)

  expect_error(pidoutcomes(y ~ x, z, df), "binary")
})

test_that("pidoutcomes errors when z not in data", {
  set.seed(42)
  N <- 50
  x <- rnorm(N)
  y <- rbinom(N, 1, 0.5)
  df <- data.frame(y = y, x = x)
  z <- rbinom(N, 1, 0.7)  # z not in df

  expect_error(pidoutcomes(y ~ x, z, df), "must appear in data")
})

test_that("print.rbounds works", {
  skip_if_not_installed("np")

  set.seed(42)
  N <- 50
  x <- rnorm(N)
  z <- rbinom(N, 1, 0.7)
  y <- rbinom(N, 1, pnorm(x))
  df <- data.frame(y = y, x = x, z = z)

  res <- pidoutcomes(y ~ x, z, df)

  # print should return invisibly
  expect_output(ret <- print(res))
  expect_identical(ret, res)
})
