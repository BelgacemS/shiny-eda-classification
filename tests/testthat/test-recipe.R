# Expected values are computed by hand in the comments, never by the recipe itself.

recipe_with <- function(...) utils::modifyList(default_recipe(), list(...))

sd_by_hand <- function(x) sqrt(sum((x - sum(x) / length(x))^2) / length(x))

test_that("standard scaling gives mean 0 and sd (1/n) 1 on train, train parameters on test", {
  set.seed(2)
  train <- data.frame(x = rnorm(40, 10, 3))
  test <- data.frame(x = c(-5, 10, 30))
  fitted <- fit_recipe(recipe_with(scale = list(method = "standard")), train, c(x = "quanti"))
  out <- fitted$train$x
  expect_equal(sum(out) / length(out), 0)
  expect_equal(sd_by_hand(out), 1)
  mu <- sum(train$x) / nrow(train)
  sigma <- sd_by_hand(train$x)
  expect_equal(apply_recipe(fitted, test)$x, (test$x - mu) / sigma)
})

test_that("parameters equal the values computed by hand on a toy set", {
  # x: quartiles (type 7) of 1, 2, 3, 4, 100 are 2 and 4, IQR 2, fences -1 and 7;
  # after capping x = 1, 2, 3, 4, 7: mean 17 / 5 = 3.4, variance (1/n) 21.2 / 5 = 4.24.
  # y: observed 1, 2, 6, median 2, mean 3.
  train <- data.frame(x = c(1, 2, 3, 4, 100), y = c(1, NA, 2, 6, NA))
  types <- c(x = "quanti", y = "quanti")
  recipe <- recipe_with(outliers = list(action = "cap"), impute = list(quanti = "median"),
                        scale = list(method = "standard"))
  fitted <- fit_recipe(recipe, train, types)
  expect_equal(fitted$params$outliers$bounds$x, c(lower = -1, upper = 7))
  expect_equal(unname(fitted$params$outliers$counts["x"]), 1)
  expect_equal(fitted$params$impute$y, 2)
  expect_equal(fitted$params$scale$params$x, c(center = 3.4, scale = sqrt(4.24)))
  # A test value of 50 is capped to 7 before scaling.
  expect_equal(apply_recipe(fitted, data.frame(x = 50, y = NA))$x, (7 - 3.4) / sqrt(4.24))

  mean_fit <- fit_recipe(recipe_with(impute = list(quanti = "mean")), train, types)
  expect_equal(mean_fit$params$impute$y, 3)
  expect_equal(apply_recipe(mean_fit, data.frame(x = 1, y = NA))$y, 3)

  # minmax on 1, 2, 3, 4, 100: min 1, range 99.
  minmax <- fit_recipe(recipe_with(scale = list(method = "minmax")), train["x"], types["x"])
  expect_equal(minmax$params$scale$params$x, c(center = 1, scale = 99))
  expect_equal(apply_recipe(minmax, data.frame(x = 199))$x, 2)
})

test_that("z-score bounds are mean +/- z sd (1/n)", {
  # 2, 4, 4, 4, 5, 5, 7, 9: mean 5, variance (1/n) 32 / 8 = 4, sd 2.
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expect_equal(outlier_bounds(x, "zscore", z = 1.5), c(lower = 2, upper = 8))
  expect_equal(outlier_bounds(c(x, NA), "tukey"), outlier_bounds(x, "tukey"))
})

test_that("dummy coding gives k - 1 columns with the most frequent level as reference, or k", {
  # b is the most frequent level, not the first in alphabetical order.
  train <- data.frame(g = factor(c("b", "b", "b", "a", "c")), x = 1:5)
  types <- c(g = "quali", x = "quanti")
  k1 <- fit_recipe(recipe_with(dummy = list(method = "k_minus_1")), train, types)
  expect_equal(k1$params$dummy$params$g$reference, "b")
  expect_equal(names(k1$train), c("g_a", "g_c", "x"))
  expect_equal(k1$train$g_a, c(0, 0, 0, 1, 0))
  expect_equal(unname(k1$output_types), c("quanti", "quanti", "quanti"))

  one_hot <- fit_recipe(recipe_with(dummy = list(method = "one_hot")), train, types)
  expect_equal(names(one_hot$train), c("g_a", "g_b", "g_c", "x"))
  expect_true(all(rowSums(one_hot$train[c("g_a", "g_b", "g_c")]) == 1))
})

test_that("an unknown test level goes to Autre when Autre has training rows, else to the mode", {
  train <- data.frame(g = factor(c("a", "a", "a", "a", "b", "c", "a", "a")))
  types <- c(g = "quali")
  test <- data.frame(g = factor(c("d", "b", "a")))
  # Shares a 6/8, b 1/8, c 1/8: with threshold 0.2, b and c are rare.
  rare <- fit_recipe(recipe_with(rare = list(threshold = 0.2)), train, types)
  expect_equal(levels(rare$train$g), c("a", "Autre"))
  expect_equal(as.character(apply_recipe(rare, test)$g), c("Autre", "Autre", "a"))
  # Without rare levels, Autre has no row: the unknown level goes to a, the mode.
  plain <- fit_recipe(default_recipe(), train, types)
  expect_equal(levels(plain$train$g), c("a", "b", "c"))
  out <- expect_silent(apply_recipe(plain, test))
  expect_equal(as.character(out$g), c("a", "b", "a"))
  # Dummy columns of the test only cover the training levels.
  dummies <- fit_recipe(recipe_with(dummy = list(method = "one_hot")), train, types)
  expect_equal(unlist(apply_recipe(dummies, test)[1, ], use.names = FALSE), c(1, 0, 0))
})

test_that("a setting on a column absent from train is ignored", {
  train <- data.frame(x = c(1, 2, 3), g = factor(c("a", "b", "a")))
  types <- c(x = "quanti", g = "quali", absent = "quanti")
  recipe <- recipe_with(scale = list(method = "standard", vars = c("x", "absent")),
                        rare = list(threshold = 0.5, vars = "absent"))
  fitted <- expect_silent(fit_recipe(recipe, train, types))
  expect_equal(names(fitted$params$scale$params), "x")
  expect_equal(levels(fitted$train$g), c("a", "b"))
  expect_equal(names(apply_recipe(fitted, train)), c("x", "g"))
})

test_that("a constant column is dropped with a message, without NaN", {
  train <- data.frame(x = c(1, 2, 3), k = c(5, 5, 5), e = c(NA_real_, NA, NA))
  types <- c(x = "quanti", k = "quanti", e = "quanti")
  for (method in c("standard", "minmax")) {
    fitted <- fit_recipe(recipe_with(scale = list(method = method)), train, types)
    expect_equal(names(fitted$train), "x")
    expect_match(fitted$messages, "constante.*k, e")
    out <- apply_recipe(fitted, data.frame(x = 2, k = 7, e = 1))
    expect_false(any(is.nan(unlist(out))))
  }
})

test_that("removing outliers drops training rows only", {
  train <- data.frame(x = c(1, 2, 3, 4, 100), y = c(10, 11, 12, -500, 13))
  types <- c(x = "quanti", y = "quanti")
  fitted <- fit_recipe(recipe_with(outliers = list(action = "remove")), train, types)
  # x = 100 (row 5) and y = -500 (row 4) are outside the Tukey fences.
  expect_equal(fitted$kept_rows, 1:3)
  expect_equal(nrow(fitted$train), 3L)
  test <- data.frame(x = c(1000, 2), y = c(10, -1000))
  expect_equal(apply_recipe(fitted, test), test)
})

test_that("without outlier removal, apply_recipe on train gives the output of the fit", {
  set.seed(3)
  train <- data.frame(
    x = c(rnorm(30), 15, NA),
    g = factor(c(sample(c("a", "b", "c"), 30, TRUE), "z", NA)),
    h = factor(sample(c("u", "v"), 32, TRUE)),
    id = sprintf("r%02d", 1:32)
  )
  types <- c(x = "quanti", g = "quali", h = "quali", id = "id")
  recipe <- recipe_with(outliers = list(action = "cap"),
                        impute = list(quanti = "median", quali = "missing_level"),
                        rare = list(threshold = 0.05), scale = list(method = "minmax"),
                        dummy = list(method = "k_minus_1"))
  fitted <- fit_recipe(recipe, train, types)
  expect_identical(apply_recipe(fitted, train), fitted$train)
  expect_identical(fitted$kept_rows, seq_len(nrow(train)))
  expect_false(anyNA(fitted$train))
})

test_that("missing values become a Manquant level, also on the test set", {
  train <- data.frame(g = factor(c("a", NA, "b", "a")))
  fitted <- fit_recipe(recipe_with(impute = list(quali = "missing_level")), train, c(g = "quali"))
  expect_equal(levels(fitted$train$g), c("a", "b", "Manquant"))
  expect_equal(as.character(apply_recipe(fitted, data.frame(g = NA))$g), "Manquant")
  mode <- fit_recipe(recipe_with(impute = list(quali = "mode")), train, c(g = "quali"))
  expect_equal(as.character(mode$train$g), c("a", "a", "b", "a"))
})

test_that("a variable above the column cap is dropped with a message", {
  train <- data.frame(g = factor(letters[c(1:5, 1:5)]), x = 1:10)
  recipe <- recipe_with(dummy = list(method = "one_hot", max_cols = 4))
  fitted <- fit_recipe(recipe, train, c(g = "quali", x = "quanti"))
  expect_equal(names(fitted$train), "x")
  expect_match(fitted$messages, "plus de 4 colonnes.*g")
  # k - 1 needs 4 columns: within the cap.
  k1 <- fit_recipe(recipe_with(dummy = list(method = "k_minus_1", max_cols = 4)), train,
                   c(g = "quali", x = "quanti"))
  expect_equal(ncol(k1$train), 5L)
})

test_that("id, ignore and untyped columns pass through untouched", {
  train <- data.frame(x = c(1, 2, 3), y = factor(c("non", "oui", "non")), code = c("A", "B", "C"))
  types <- c(x = "quanti", y = "ignore", code = "id")
  recipe <- recipe_with(scale = list(method = "standard"), dummy = list(method = "one_hot"))
  fitted <- fit_recipe(recipe, train, types)
  expect_identical(fitted$train$y, train$y)
  expect_identical(fitted$train$code, train$code)
  expect_equal(unname(fitted$output_types), c("quanti", "ignore", "id"))
})

test_that("apply_recipe refuses data without a fitted column", {
  fitted <- fit_recipe(default_recipe(), data.frame(x = 1:3, z = 1:3), c(x = "quanti", z = "quanti"))
  expect_error(apply_recipe(fitted, data.frame(x = 1)), "absentes.*z")
})
