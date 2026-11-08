# Extracted from test-models.R:625

# prequel ----------------------------------------------------------------------
simulated <- function(n = 400, seed = 1) {
  withr::local_seed(seed)
  x <- rnorm(n)
  g <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  y <- rbinom(n, 1, stats::plogis(1.5 * x + (g == "b")))
  list(x = data.frame(x = x, g = g), y = y)
}
pipeline_data <- function(n_acc = 300, seed = 12) {
  withr::local_seed(seed)
  acc <- rep(seq_len(n_acc), each = 3)
  n <- length(acc)
  x <- rnorm(n)
  data <- data.frame(
    y = factor(ifelse(stats::rbinom(n, 1, stats::plogis(2 * x - 1)) == 1, "oui", "non")),
    x = x, z = rnorm(n, 100, 10), g = factor(sample(c("a", "b", "c"), n, TRUE)),
    acc = as.character(acc), year = rep(sample(2019:2024, n_acc, TRUE), each = 3))
  make_dataset(data, c(y = "quali", x = "quanti", z = "quanti", g = "quali", acc = "id",
                       year = "quanti"))
}
pipeline_settings <- function(...) {
  utils::modifyList(list(target = "y", positive = "oui", predictors = c("x", "z", "g"),
                         sample_size = 10000, group = "acc", mode = "grouped", test_prop = 0.2,
                         valid_prop = 0.25, models = c("glm", "ranger"), seed = 1), list(...))
}
nested_data <- function(n = 400, seed = 40) {
  withr::local_seed(seed)
  x <- data.frame(a = rnorm(n), f = factor(sample(c("p", "q", "r"), n, TRUE)))
  # g is nested in f (g = "P" exactly when f = "p"): one coefficient is not estimable.
  x$g <- factor(ifelse(x$f == "p", "P", "autre"))
  list(x = x, y = stats::rbinom(n, 1, stats::plogis(x$a)))
}

# test -------------------------------------------------------------------------
withr::local_seed(42)
n <- 600
f <- factor(sample(c("p", "q", "r"), n, TRUE))
data <- data.frame(y = factor(ifelse(stats::rbinom(n, 1, 0.35) == 1, "oui", "non")),
                     a = rnorm(n), f = f, g = factor(ifelse(f == "p", "P", "autre")))
ds <- make_dataset(data, c(y = "quali", a = "quanti", f = "quali", g = "quali"))
out <- train_pipeline(ds, default_recipe(), list(
    target = "y", positive = "oui", predictors = c("a", "f", "g"), sample_size = n, group = NULL,
    mode = "stratified", test_prop = 0.2, valid_prop = 0.25, models = "glm", seed = 1))
expect_false(any(grepl("non estimable", out$results$glm$messages)))
