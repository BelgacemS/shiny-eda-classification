# Expected values computed by hand, or from base R corrected by (n - 1) / n.

test_that("var_1n divides by n", {
  # mean 2.5, squared deviations 2.25 + 0.25 + 0.25 + 2.25 = 5, 5 / 4
  expect_equal(var_1n(c(1, 2, 3, 4)), 1.25)
  expect_equal(mean_1n(c(1, 2, 3, 4)), 2.5)
  expect_equal(sd_1n(c(1, 2, 3, 4)), sqrt(1.25))
})

test_that("1/n statistics match base R corrected by (n - 1) / n", {
  set.seed(1)
  x <- rnorm(30)
  y <- x + rnorm(30)
  n <- length(x)
  expect_equal(var_1n(x), var(x) * (n - 1) / n)
  expect_equal(sd_1n(x), sd(x) * sqrt((n - 1) / n))
  expect_equal(cov_1n(x, y), cov(x, y) * (n - 1) / n)
  expect_equal(cov_1n(x, x), var_1n(x))
})

test_that("a constant vector has zero variance and NA propagates", {
  expect_equal(var_1n(rep(3, 5)), 0)
  expect_true(is.na(mean_1n(c(1, NA))))
})

test_that("cov_1n refuses vectors of different lengths", {
  expect_error(cov_1n(1:3, 1:4), "same length")
})

test_that("is_constant reads constancy on the values", {
  expect_true(is_constant(rep(0.1, 50)))
  expect_true(is_constant(1 / 3))
  expect_true(is_constant(numeric(0)))
  expect_false(is_constant(c(0.1, 0.1 + 1e-15)))
  expect_false(is_constant(c(1, 2)))
})
