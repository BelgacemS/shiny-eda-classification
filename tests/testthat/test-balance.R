toy_split <- function(n_pos = 10, n_neg = 90) {
  part <- function(n_pos, n_neg) {
    y <- c(rep(1L, n_pos), rep(0L, n_neg))
    list(x = data.frame(a = seq_along(y), g = factor(rep(c("u", "v"), length.out = length(y)))),
         y = y)
  }
  list(train = part(n_pos, n_neg), valid = part(3, 27), test = part(4, 36))
}

class_ratio <- function(y) {
  counts <- table(y)
  min(counts) / max(counts)
}

test_that("class weights are n / (K n_c), computed by hand", {
  # n = 4, K = 2: class 0 has 3 rows, 4 / (2 * 3) = 2/3; class 1 has 1 row, 4 / 2 = 2.
  y <- c(0, 0, 0, 1)
  expect_equal(class_weights(y), c(`0` = 2 / 3, `1` = 2))
  expect_equal(observation_weights(y), c(2 / 3, 2 / 3, 2 / 3, 2))
  # Each class weighs n / K in total.
  expect_equal(sum(observation_weights(y)[y == 0]), 2)
  # Three classes: 6 rows, counts 3, 2, 1: weights 6/9, 6/6, 6/3.
  expect_equal(class_weights(c("a", "a", "a", "b", "b", "c")), c(a = 2 / 3, b = 1, c = 2))
})

test_that("the three methods leave valid and test identical", {
  split <- toy_split()
  for (method in c("weights", "under", "over")) {
    out <- balance_train(split, method, ratio = 0.5, seed = 3)
    expect_identical(out$valid, split$valid)
    expect_identical(out$test, split$test)
  }
  none <- balance_train(split, "none")
  expect_identical(none$train$x, split$train$x)
  expect_null(none$train$weights)
})

test_that("the class ratio is reached after resampling", {
  split <- toy_split()
  # 10 positives and 90 negatives; ratio 0.5: under keeps 20 negatives, over grows positives to 45.
  under <- balance_train(split, "under", ratio = 0.5)
  expect_equal(as.vector(table(under$train$y)), c(20L, 10L))
  expect_equal(class_ratio(under$train$y), 0.5)
  over <- balance_train(split, "over", ratio = 0.5)
  expect_equal(as.vector(table(over$train$y)), c(90L, 45L))
  expect_equal(class_ratio(over$train$y), 0.5)
  # x and y stay aligned: in the toy set, a positive row has a <= 10.
  expect_true(all((over$train$x$a <= 10) == (over$train$y == 1)))
  expect_true(all((under$train$x$a <= 10) == (under$train$y == 1)))
  expect_equal(nrow(under$train$x), length(under$train$y))
  # Every minority row is kept, and oversampling keeps every original row.
  expect_true(all(1:10 %in% under$train$x$a))
  expect_true(all(1:100 %in% over$train$x$a))
  # Balanced by default.
  expect_equal(class_ratio(balance_train(split, "under")$train$y), 1)
  expect_equal(class_ratio(balance_train(split, "over")$train$y), 1)
})

test_that("weights balance the total weight of the classes", {
  out <- balance_train(toy_split(), "weights")
  w <- out$train$weights
  expect_length(w, 100L)
  expect_equal(sum(w[out$train$y == 1]), sum(w[out$train$y == 0]))
  expect_identical(out$train$x, toy_split()$train$x)
})

test_that("same seed, same result; the global random state is untouched", {
  y <- c(rep(1, 10), rep(0, 90))
  expect_identical(undersample(y, 0.5, seed = 7), undersample(y, 0.5, seed = 7))
  expect_identical(oversample(y, 0.5, seed = 7), oversample(y, 0.5, seed = 7))
  expect_false(identical(undersample(y, 0.5, seed = 7), undersample(y, 0.5, seed = 8)))
  set.seed(42)
  before <- .Random.seed
  balance_train(toy_split(), "over", seed = 1)
  expect_identical(.Random.seed, before)
})

test_that("a ratio already reached changes nothing", {
  y <- c(rep(1, 40), rep(0, 60))
  expect_equal(undersample(y, ratio = 0.5), 1:100)
  expect_equal(oversample(y, ratio = 0.5), 1:100)
})

test_that("invalid inputs are refused with a message", {
  y <- c(1, 0, 0)
  expect_error(undersample(y, ratio = 0), "ratio")
  expect_error(oversample(y, ratio = 1.5), "ratio")
  expect_error(undersample(c("a", "b", "c")), "deux classes")
  expect_error(class_weights(c(1, NA)), "manquante")
  expect_error(balance_train(toy_split(), "smote"), "inconnue")
})

test_that("with_seed restores a missing seed and survives an error", {
  env <- globalenv()
  old <- if (exists(".Random.seed", envir = env)) get(".Random.seed", envir = env)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = env))
  if (exists(".Random.seed", envir = env)) rm(".Random.seed", envir = env)
  with_seed(1, runif(1))
  expect_false(exists(".Random.seed", envir = env))
  set.seed(5)
  before <- .Random.seed
  expect_error(with_seed(1, stop("boom")), "boom")
  expect_identical(.Random.seed, before)
})
