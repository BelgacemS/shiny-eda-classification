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
  # One group of at least two rows gets a row in 2024 and the others in 2023.
  d <- simulate(n_groups = 200)
  sizes <- table(d$groups)
  rows <- which(d$groups == names(sizes)[sizes >= 2][1])
  d$x$year[rows] <- 2023
  d$x$year[rows[1]] <- 2024
  s <- split_data(d$x, d$y, "variable", groups = d$groups, by = d$x$year, test_values = 2024, seed = 1)
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
  d <- simulate(n_groups = 300)
  d$groups[seq(1, length(d$groups), by = 3)] <- NA
  s <- split_data(d$x, d$y, "grouped", groups = d$groups, seed = 2)
  expect_equal(all_rows(s), seq_along(d$y))
  # Named groups stay on one side.
  named <- lapply(s, function(part) unique(stats::na.omit(d$groups[part$rows])))
  expect_length(intersect(named$train, named$test), 0)
  expect_length(intersect(named$train, named$valid), 0)
  # Rows without group id are not one big group: they are spread over the parts.
  na_rows <- which(is.na(d$groups))
  sides <- vapply(s, function(part) any(part$rows %in% na_rows), logical(1))
  expect_true(all(sides))
})

test_that("the stratified mode works on rows alone and ignores groups", {
  d <- simulate()
  s <- split_data(d$x, d$y, "stratified", seed = 4)
  p <- vapply(s, prevalence, numeric(1))
  expect_lte(max(p) - min(p), 0.02)
  expect_identical(s, split_data(d$x, d$y, "stratified", seed = 4))
  expect_identical(s, split_data(d$x, d$y, "stratified", groups = d$groups, seed = 4))
  expect_false(identical(s$test$rows, split_data(d$x, d$y, "stratified", seed = 5)$test$rows))
})

test_that("empty parts or a single class in train or validation stop the split", {
  x <- data.frame(a = 1:13)
  expect_error(split_data(x[1:2, , drop = FALSE], c(1, 0)), "vide")
  # 3 positives among 13 rows: no positive left for the validation set.
  expect_error(split_data(x, c(1, 1, 1, rep(0, 10)), seed = 1), "une classe|vide")
  d <- simulate(n_groups = 200)
  expect_error(split_data(d$x, d$y, "variable", by = d$x$year, test_values = 2030), "test est vide")
  expect_error(split_data(d$x, d$y, "variable", by = replace(d$x$year, 1, NA), test_values = 2024),
               "manquantes")
  expect_error(split_data(d$x, d$y, test_prop = 0), "proportions")
})

test_that("sample_rows draws whole groups until the size is reached", {
  d <- simulate()
  rows <- sample_rows(length(d$y), 1000, groups = d$groups, seed = 3)
  expect_gte(length(rows), 1000)
  expect_lte(length(rows), 1003)  # at most one group of 4 rows beyond the size
  chosen <- unique(d$groups[rows])
  expect_equal(sort(rows), which(d$groups %in% chosen))
  expect_identical(rows, sample_rows(length(d$y), 1000, groups = d$groups, seed = 3))
  plain <- sample_rows(1000, 100, seed = 1)
  expect_length(plain, 100)
  # A random draw, not the first rows (a file sorted by year would give one year).
  expect_false(identical(plain, 1:100))
  expect_false(identical(plain, sample_rows(1000, 100, seed = 2)))
  expect_gt(max(plain), 500)
  expect_equal(sample_rows(10, 50), 1:10)
  # Groups of 2 rows and a size of 10: exactly 10 rows, the draw stops as soon
  # as the size is reached.
  expect_length(sample_rows(40, 10, groups = rep(1:20, each = 2), seed = 1), 10)
  expect_error(sample_rows(10, 0), "taille")
  expect_error(sample_rows(10, 5, groups = 1:3), "groupe")
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
