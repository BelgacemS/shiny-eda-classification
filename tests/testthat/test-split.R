simulate <- function(n_groups = 2000, prevalence = 0.18, seed = 1) {
  withr::local_seed(seed)
  sizes <- sample(1:4, n_groups, replace = TRUE)
  groups <- rep(sprintf("g%05d", seq_len(n_groups)), sizes)
  n <- length(groups)
  y <- rbinom(n, 1, prevalence)
  # The year is a property of the group, as for BAAC accidents.
  year <- rep(sample(2019:2024, n_groups, replace = TRUE), sizes)
  x <- data.frame(a = rnorm(n), year = year, id = seq_len(n))
  list(x = x, y = y, groups = groups)
}

prevalence <- function(part) mean(part$y)

all_rows <- function(split) sort(c(split$train$rows, split$valid$rows, split$test$rows))

test_that("the stratified split covers every row once with the requested sizes", {
  d <- simulate()
  s <- split_data(d$x, d$y, "stratified", test_prop = 0.2, valid_prop = 0.25, seed = 3)
  expect_equal(all_rows(s), seq_along(d$y))
  n <- length(d$y)
  expect_equal(length(s$test$y), round(sum(d$y == 1) * 0.2) + round(sum(d$y == 0) * 0.2))
  expect_equal(length(s$valid$y) / (n - length(s$test$y)), 0.25, tolerance = 0.01)
  # x, y and rows stay aligned.
  expect_equal(s$test$x$id, s$test$rows)
  expect_equal(s$train$y, d$y[s$train$rows])
})

test_that("the proportion of positives stays within 2 points across the parts", {
  d <- simulate()
  for (mode in c("stratified", "grouped")) {
    s <- split_data(d$x, d$y, mode, groups = d$groups, seed = 4)
    p <- vapply(s, prevalence, numeric(1))
    expect_lte(max(p) - min(p), 0.02)
  }
  s <- split_data(d$x, d$y, "variable", by = d$x$year, test_values = 2024, seed = 4)
  expect_lte(abs(prevalence(s$train) - prevalence(s$valid)), 0.02)
})

test_that("no group is in two parts, in grouped and variable modes", {
  d <- simulate()
  check <- function(s) {
    g <- lapply(s, function(part) unique(d$groups[part$rows]))
    expect_length(intersect(g$train, g$valid), 0)
    expect_length(intersect(g$train, g$test), 0)
    expect_length(intersect(g$valid, g$test), 0)
  }
  check(split_data(d$x, d$y, "grouped", groups = d$groups, seed = 5))
  # By variable with groups: the year belongs to the group, so no group is cut,
  # and the validation set is grouped too.
  s <- split_data(d$x, d$y, "variable", groups = d$groups, by = d$x$year, test_values = 2024, seed = 5)
  check(s)
  expect_equal(attr(s, "crossing_groups"), 0L)
})

test_that("groups cut by the test values of the variable mode are counted", {
  # Group "a" has a row in 2024 and a row in 2023.
  x <- data.frame(year = c(2024, 2023, 2023, 2024, 2023, 2023))
  s <- split_data(x, c(1, 0, 1, 0, 1, 0), "variable", groups = c("a", "a", "b", "c", "d", "d"),
                  by = x$year, test_values = 2024, seed = 1)
  expect_equal(attr(s, "crossing_groups"), 1L)
})

test_that("grouped split stratifies on groups that contain a positive", {
  d <- simulate()
  s <- split_data(d$x, d$y, "grouped", groups = d$groups, test_prop = 0.2, seed = 6)
  positive_groups <- unique(d$groups[d$y == 1])
  test_groups <- unique(d$groups[s$test$rows])
  share <- mean(positive_groups %in% test_groups)
  expect_equal(share, 0.2, tolerance = 0.01)
})

test_that("same seed, same split; another seed, another split", {
  d <- simulate()
  for (mode in c("stratified", "grouped")) {
    a <- split_data(d$x, d$y, mode, groups = d$groups, seed = 7)
    b <- split_data(d$x, d$y, mode, groups = d$groups, seed = 7)
    expect_identical(a, b)
    c <- split_data(d$x, d$y, mode, groups = d$groups, seed = 8)
    expect_false(identical(a$test$rows, c$test$rows))
  }
  withr::local_seed(42)
  before <- .Random.seed
  split_data(d$x, d$y, "stratified", seed = 1)
  expect_identical(.Random.seed, before)
})

test_that("the variable mode puts exactly the rows with the test values in test", {
  d <- simulate()
  s <- split_data(d$x, d$y, "variable", by = d$x$year, test_values = c(2023, 2024), seed = 9)
  expect_equal(s$test$rows, which(d$x$year %in% c(2023, 2024)))
  expect_false(any(d$x$year[c(s$train$rows, s$valid$rows)] %in% c(2023, 2024)))
  expect_equal(all_rows(s), seq_along(d$y))
})

test_that("a missing group id makes its row a group of its own", {
  y <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
  groups <- c("a", "a", NA, NA, NA, "b", "b", NA, "c", "c")
  s <- split_data(data.frame(i = 1:10), y, "grouped", groups = groups, test_prop = 0.5, seed = 2)
  expect_equal(all_rows(s), 1:10)
  for (g in c("a", "b", "c")) {
    sides <- vapply(s, function(part) any(groups[part$rows] %in% g), logical(1))
    expect_equal(sum(sides), 1)
  }
})

test_that("sample_rows draws whole groups until the size is reached", {
  d <- simulate()
  rows <- sample_rows(length(d$y), 1000, groups = d$groups, seed = 3)
  expect_gte(length(rows), 1000)
  expect_lte(length(rows), 1003)  # at most one group of 4 rows beyond the size
  chosen <- unique(d$groups[rows])
  expect_equal(sort(rows), which(d$groups %in% chosen))
  expect_identical(rows, sample_rows(length(d$y), 1000, groups = d$groups, seed = 3))
  expect_equal(length(sample_rows(100, 30, seed = 1)), 30)
  expect_equal(sample_rows(10, 50), 1:10)
})

test_that("invalid inputs are refused with a message", {
  x <- data.frame(a = 1:4)
  expect_error(split_data(x, c(1, 0, 1, 2)), "0 ou 1")
  expect_error(split_data(x, c(1, 0, NA, 0)), "0 ou 1")
  expect_error(split_data(x, c(1, 0, 1, 0), "grouped"), "groupe")
  expect_error(split_data(x, c(1, 0, 1, 0), "variable"), "variable")
  expect_error(split_data(x, c(1, 0, 1, 0), test_prop = 1), "proportions")
  expect_error(split_data(x, c(1, 0, 1, 0), "random"), "inconnu")
})
