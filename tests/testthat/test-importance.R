# E11b: permutation importance, logistic coefficients, rebalancing in the pipeline.

importance_data <- function(n = 1500, seed = 30) {
  withr::local_seed(seed)
  x <- data.frame(x1 = rnorm(n), noise = rnorm(n), g = factor(sample(c("a", "b", "c"), n, TRUE)))
  y <- stats::rbinom(n, 1, stats::plogis(2 * x$x1 - 1))
  list(x = x, y = y)
}

test_that("permutation importance finds the only informative variable", {
  train <- importance_data(seed = 30)
  test <- importance_data(n = 800, seed = 31)
  for (type in c("glm", "ranger")) {
    model <- fit_model(type, train$x, train$y)
    imp <- perm_importance(function(x) predict_proba(model, x), test$x, test$y,
                           names(test$x), n_rep = 3, seed = 1)
    expect_equal(imp$variable[1], "x1", info = type)
    expect_gt(imp$importance[imp$variable == "x1"], 0.2)
    expect_lt(abs(imp$importance[imp$variable == "noise"]), 0.02)
    expect_lt(abs(imp$importance[imp$variable == "g"]), 0.02)
  }
})

test_that("permutation importance equals the AUC drop computed by hand", {
  test <- importance_data(n = 300, seed = 32)
  model <- fit_model("glm", test$x, test$y)
  fn <- function(x) predict_proba(model, x)
  imp <- perm_importance(fn, test$x, test$y, "x1", n_rep = 1, seed = 7)
  order <- with_seed(7, sample.int(nrow(test$x)))
  permuted <- test$x
  permuted$x1 <- test$x$x1[order]
  expected <- roc_curve(test$y, fn(test$x))$auc - roc_curve(test$y, fn(permuted))$auc
  expect_equal(imp$importance, expected)
})

test_that("permutation importance is reproducible and leaves the global random state alone", {
  test <- importance_data(n = 300, seed = 33)
  model <- fit_model("glm", test$x, test$y)
  fn <- function(x) predict_proba(model, x)
  set.seed(5)
  state <- .Random.seed
  a <- perm_importance(fn, test$x, test$y, names(test$x), n_rep = 2, seed = 3)
  expect_identical(.Random.seed, state)
  b <- perm_importance(fn, test$x, test$y, names(test$x), n_rep = 2, seed = 3)
  expect_identical(a, b)
  expect_false(anyNA(a$sd))
  expect_true(all(is.na(perm_importance(fn, test$x, test$y, "x1", n_rep = 1)$sd)))
  expect_error(perm_importance(fn, test$x, rep(1, 300), "x1"), "une classe")
})

test_that("the importance rows are all the test rows up to the size, a sample beyond", {
  expect_equal(importance_rows(50, 100), 1:50)
  rows <- importance_rows(5000, 1000, seed = 2)
  expect_length(rows, 1000)
  expect_false(is.unsorted(rows))
  expect_identical(rows, importance_rows(5000, 1000, seed = 2))
})

test_that("standardized coefficients use the 1/n sd of the design columns and odds ratios the Wald intervals", {
  d <- importance_data(n = 600, seed = 34)
  model <- fit_model("glm", d$x, d$y)
  li <- logistic_importance(model, d$x)
  beta <- stats::coef(model$fit)
  # x1 is numeric: its design column is x1 itself.
  row <- li[li$term == "x1", ]
  n <- nrow(d$x)
  expect_equal(row$sd, stats::sd(d$x$x1) * sqrt((n - 1) / n))
  expect_equal(row$standardized, abs(beta[["x1"]]) * row$sd)
  # A dummy column: its 1/n sd is sqrt(p (1 - p)), p the share of the level.
  level <- grep("^g", li$term, value = TRUE)[1]
  p <- mean(as.character(d$x$g) == sub("^g", "", level))
  expect_equal(li$sd[li$term == level], sqrt(p * (1 - p)))
  ci <- exp(stats::confint.default(model$fit))
  expect_equal(row$odds_ratio, exp(beta[["x1"]]))
  expect_equal(c(row$lower, row$upper), unname(ci["x1", ]))
  expect_false("(Intercept)" %in% li$term)
  expect_false(is.unsorted(rev(li$standardized)))
})

pipeline_frame <- function(n = 900, seed = 35) {
  withr::local_seed(seed)
  x <- rnorm(n)
  data <- data.frame(y = factor(ifelse(stats::rbinom(n, 1, stats::plogis(2 * x - 1.8)) == 1, "oui", "non")),
                     x = x, noise = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  make_dataset(data, c(y = "quali", x = "quanti", noise = "quanti", g = "quali"))
}

frame_settings <- function(...) {
  utils::modifyList(list(target = "y", positive = "oui", predictors = c("x", "noise", "g"),
                         sample_size = 10000, group = NULL, mode = "stratified", test_prop = 0.2,
                         valid_prop = 0.25, models = c("glm", "ranger", "rpart"), seed = 1),
                    list(...))
}

test_that("validation and test are identical with and without rebalancing", {
  ds <- pipeline_frame()
  plain <- train_pipeline(ds, default_recipe(), frame_settings())
  for (method in c("weights", "under", "over")) {
    balanced <- train_pipeline(ds, default_recipe(), frame_settings(balance = method))
    expect_identical(balanced$split$valid, plain$split$valid, info = method)
    expect_identical(balanced$split$test, plain$split$test, info = method)
    expect_identical(balanced$distribution, plain$distribution, info = method)
    # The models change: they are trained on the rebalanced training set.
    expect_false(identical(balanced$results$glm$prob_valid, plain$results$glm$prob_valid), info = method)
  }
})

test_that("the rebalanced training set is reported and used by the models", {
  ds <- pipeline_frame()
  plain <- train_pipeline(ds, default_recipe(), frame_settings())
  n_train <- plain$distribution$n[1]
  positives <- plain$distribution$positives[1]
  expect_equal(plain$results$glm$balance$method, "none")
  under <- train_pipeline(ds, default_recipe(), frame_settings(balance = "under"))$results$glm
  expect_equal(under$balance$positives, positives)
  expect_equal(under$balance$n, 2 * positives)
  expect_equal(nrow(under$model$fit$model), 2 * positives)
  over <- train_pipeline(ds, default_recipe(), frame_settings(balance = "over", balance_ratio = 0.5))$results$glm
  expect_equal(over$balance$n - over$balance$positives, n_train - positives)
  expect_equal(over$balance$positives, round(0.5 * (n_train - positives)))
  weighted <- train_pipeline(ds, default_recipe(), frame_settings(balance = "weights"))$results
  expect_equal(weighted$glm$balance$share, 0.5)
  expect_equal(weighted$glm$balance$n, n_train)
  expect_equal(weighted$glm$model$fit$family$family, "quasibinomial")
  expect_equal(unname(weighted$glm$model$fit$prior.weights),
               observation_weights(plain$split$train$y))
})

test_that("importances use the test rows, and odds ratios come from an unweighted fit", {
  ds <- pipeline_frame()
  out <- train_pipeline(ds, default_recipe(), frame_settings(balance = "weights"))
  imp <- compute_importances(out, size = 100, n_rep = 1, seed = 1)
  expect_equal(names(imp$permutation), c("glm", "ranger", "rpart"))
  expect_equal(imp$n_rows, 100L)
  # Same values as perm_importance on the same test rows.
  rows <- importance_rows(length(out$split$test$y), 100, 1)
  r <- out$results$ranger
  direct <- perm_importance(function(x) predict_proba(r$model, apply_recipe(r$recipe, x)),
                            out$split$test$x[rows, , drop = FALSE], out$split$test$y[rows],
                            names(out$split$test$x), 1, 1)
  expect_equal(imp$permutation$ranger, direct)
  # Standardized coefficients from the weighted model, odds ratios from an unweighted refit.
  weighted_beta <- stats::coef(out$results$glm$model$fit)
  unweighted <- fit_model("glm", out$results$glm$recipe$train,
                          out$split$train$y[out$results$glm$recipe$kept_rows])
  row <- imp$logistic[imp$logistic$term == "x", ]
  expect_equal(row$standardized, abs(weighted_beta[["x"]]) * row$sd)
  expect_equal(row$odds_ratio, exp(stats::coef(unweighted$fit)[["x"]]))
  expect_false(isTRUE(all.equal(row$odds_ratio, exp(weighted_beta[["x"]]))))
  # Without rebalancing the model itself gives the odds ratios.
  plain <- train_pipeline(ds, default_recipe(), frame_settings(models = "glm"))
  li <- compute_importances(plain, size = 100)$logistic
  expect_equal(li$odds_ratio[li$term == "x"], exp(stats::coef(plain$results$glm$model$fit)[["x"]]))
})

test_that("no importance is computed before the test button, and a new training clears them", {
  ds <- pipeline_frame()
  shiny::testServer(mod_model_server,
                    args = list(dataset_clean = shiny::reactive(ds),
                                recipe = shiny::reactive(default_recipe())), {
    session$setInputs(target = "y", positive = "oui", predictors = c("x", "noise", "g"),
                      sample_size = 900, group = NO_GROUP, mode = "stratified", test_prop = 0.2,
                      valid_prop = 0.25, models = c("glm", "rpart"), balance = "under",
                      balance_ratio = 1, seed = 1, importance_size = 150, importance_reps = 1,
                      train = 1)
    expect_false(is.null(trained()))
    expect_null(importances())
    expect_match(output$distribution, "Entraînement rééquilibré")
    session$setInputs(evaluate = 1)
    imp <- importances()
    expect_equal(names(imp$permutation), c("glm", "rpart"))
    expect_equal(imp$n_rows, 150L)
    expect_match(output$importance_info, "150 lignes du test")
    expect_match(output$logistic_table, "Odds ratio")
    # Invalid importance settings: test evaluated, importances refused with a message.
    session$setInputs(importance_reps = 9, evaluate = 2)
    expect_false(is.null(tested()))
    expect_null(importances())
    session$setInputs(importance_reps = 1, evaluate = 3)
    expect_false(is.null(importances()))
    session$setInputs(train = 2)
    expect_null(importances())
  })
})

test_that("a level lost by resampling is predicted as the most frequent training level", {
  withr::local_seed(36)
  n <- 600
  g <- factor(c(rep("rare", 3), sample(c("a", "b"), n - 3, TRUE, prob = c(0.7, 0.3))))
  x <- data.frame(g = g, v = rnorm(n))
  y <- c(0, 0, 0, stats::rbinom(n - 3, 1, 0.3))
  kept <- 4:n  # as if resampling had removed the three "rare" rows
  for (type in names(MODEL_LABELS)) {
    model <- fit_model(type, x[kept, ], y[kept])
    expect_equal(model$fallback$g, list(levels = "rare", to = "a"), info = type)
    expect_match(model$messages, "modalités sans ligne.*g \\(rare\\)", all = FALSE)
    as_a <- x[1:3, ]
    as_a$g <- factor("a", levels = levels(g))
    expect_equal(predict_proba(model, x[1:3, ]), predict_proba(model, as_a), info = type)
    # A level absent from the data used to fit stays an error for the logistic regression and the tree.
    unknown <- x[1, ]
    unknown$g <- factor("zzz")
    if (type != "ranger") expect_error(predict_proba(model, unknown), "modalité inconnue")
  }
})

test_that("undersampling that removes a rare level still trains and predicts", {
  withr::local_seed(37)
  n <- 1200
  data <- data.frame(y = factor(ifelse(stats::rbinom(n, 1, 0.15) == 1, "oui", "non")), x = rnorm(n),
                     g = factor(sample(c("a", "b", "c"), n, TRUE)))
  # A rare level carried by a few negative rows only: undersampling the
  # majority class drops it from the training set.
  data$g <- factor(as.character(data$g), levels = c("a", "b", "c", "rare"))
  negatives <- which(data$y == "non")
  data$g[negatives[1:6]] <- "rare"
  ds <- make_dataset(data, c(y = "quali", x = "quanti", g = "quali"))
  s <- list(target = "y", positive = "oui", predictors = c("x", "g"), sample_size = n, group = NULL,
            mode = "stratified", test_prop = 0.2, valid_prop = 0.25, models = names(MODEL_LABELS),
            seed = 1, balance = "under", balance_ratio = 1)
  found <- FALSE
  for (seed in 1:20) {
    s$seed <- seed
    out <- train_pipeline(ds, default_recipe(), s)
    if (length(out$results$glm$model$fallback) > 0) {
      found <- TRUE
      break
    }
  }
  expect_true(found)
  thr <- vapply(out$results, `[[`, numeric(1), "threshold")
  expect_silent(res <- evaluate_test(out, thr))
  expect_length(compute_importances(out, size = 100)$permutation, 3L)
})

test_that("a quasi-separated level gets a huge Wald standard error and is flagged", {
  withr::local_seed(38)
  n <- 800
  x <- data.frame(v = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  y <- stats::rbinom(n, 1, stats::plogis(x$v))
  # Level "s": 4 rows, all positive: the coefficient runs away.
  x$g <- factor(as.character(x$g), levels = c("a", "b", "s"))
  x$g[1:4] <- "s"
  y[1:4] <- 1
  model <- suppressWarnings(fit_model("glm", x, y))
  li <- logistic_importance(model, x)
  ci <- log(c(li$lower[li$term == "v"], li$upper[li$term == "v"]))
  expect_equal(li$se[li$term == "v"], diff(ci) / (2 * stats::qnorm(0.975)))
  expect_gt(li$se[li$term == "gs"], SEPARATION_SE)
  expect_lt(li$se[li$term == "v"], 1)
  table <- logistic_table(li)
  expect_equal(table$Remarque[table$Terme == "gs"], "séparation probable")
  expect_equal(table$Remarque[table$Terme == "v"], "")
})

# --- Tests added after review and mutation testing -------------------------------

review_data <- function(n = 300, seed = 40) {
  withr::local_seed(seed)
  x <- data.frame(x1 = rnorm(n), noise = rnorm(n))
  list(x = x, y = stats::rbinom(n, 1, stats::plogis(2 * x$x1 - 1)))
}

review_frame <- function(n = 900, seed = 35) {
  withr::local_seed(seed)
  x <- rnorm(n)
  data <- data.frame(y = factor(ifelse(stats::rbinom(n, 1, stats::plogis(2 * x - 1.8)) == 1, "oui", "non")),
                     x = x, noise = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  make_dataset(data, c(y = "quali", x = "quanti", noise = "quanti", g = "quali"))
}

review_settings <- function(...) {
  utils::modifyList(list(target = "y", positive = "oui", predictors = c("x", "noise", "g"),
                         sample_size = 10000, group = NULL, mode = "stratified", test_prop = 0.2,
                         valid_prop = 0.25, models = c("glm", "ranger", "rpart"), seed = 1),
                    list(...))
}

# Kills M04 (same permutation for every variable) and M05 (same permutation
# for every repetition).
test_that("each variable and each repetition gets its own permutation", {
  d <- review_data()
  model <- fit_model("glm", d$x, d$y)
  fn <- function(x) predict_proba(model, x)
  imp <- perm_importance(fn, d$x, d$y, c("x1", "noise"), n_rep = 2, seed = 9)
  n <- nrow(d$x)
  # Draw order: x1 rep 1, x1 rep 2, noise rep 1, noise rep 2.
  orders <- with_seed(9, lapply(1:4, function(i) sample.int(n)))
  base <- roc_curve(d$y, fn(d$x))$auc
  drop <- function(v, o) {
    p <- d$x
    p[[v]] <- d$x[[v]][o]
    base - roc_curve(d$y, fn(p))$auc
  }
  x1 <- c(drop("x1", orders[[1]]), drop("x1", orders[[2]]))
  noise <- c(drop("noise", orders[[3]]), drop("noise", orders[[4]]))
  expect_equal(imp$importance[imp$variable == "x1"], mean(x1))
  expect_equal(imp$importance[imp$variable == "noise"], mean(noise))
  # Spread over the repetitions, 1/n: half the gap between the two drops.
  expect_equal(imp$sd[imp$variable == "x1"], abs(x1[1] - x1[2]) / 2)
})

# Kills M03 (permutation of the recipe's output columns).
test_that("importances permute the original variables even when the recipe creates columns", {
  ds <- review_frame()
  rec <- default_recipe()
  rec$dummy$method <- "one_hot"
  rec$scale$method <- "standard"
  out <- train_pipeline(ds, rec, review_settings(models = c("glm", "ranger")))
  expect_false(setequal(names(out$results$ranger$recipe$train), c("x", "noise", "g")))
  imp <- compute_importances(out, size = 150)
  for (type in names(imp$permutation)) {
    expect_setequal(imp$permutation[[type]]$variable, c("x", "noise", "g"))
  }
})

# Kills M10 (s_j on the resampled training set).
test_that("s_j is computed on the training set after the recipe, not on the resampled one", {
  ds <- review_frame()
  out <- train_pipeline(ds, default_recipe(), review_settings(models = "glm", balance = "under"))
  li <- compute_importances(out, size = 100)$logistic
  expect_equal(li$sd[li$term == "x"], sd_1n(out$results$glm$recipe$train$x))
})

# Kills M15 (kept_rows ignored in the unweighted refit).
test_that("the unweighted refit uses the target aligned with the rows kept by the recipe", {
  ds <- review_frame()
  rec <- default_recipe()
  rec$outliers$action <- "remove"
  rec$outliers$method <- "zscore"
  rec$outliers$z <- 2
  out <- train_pipeline(ds, rec, review_settings(models = "glm", balance = "weights"))
  r <- out$results$glm
  expect_lt(length(r$recipe$kept_rows), length(out$split$train$y))
  li <- compute_importances(out, size = 100)$logistic
  refit <- fit_model("glm", r$recipe$train, out$split$train$y[r$recipe$kept_rows])
  expect_equal(li$odds_ratio[li$term == "x"], exp(stats::coef(refit$fit)[["x"]]))
})

# Kills M20 (weights not passed to ranger).
test_that("every model is trained with the class weights", {
  ds <- review_frame()
  plain <- train_pipeline(ds, default_recipe(), review_settings())
  weighted <- train_pipeline(ds, default_recipe(), review_settings(balance = "weights"))
  for (type in c("glm", "ranger", "rpart")) {
    expect_false(isTRUE(all.equal(weighted$results[[type]]$prob_valid, plain$results[[type]]$prob_valid)),
                 info = type)
  }
})

# Kills M32 (seed read from the current input) and M34 (weighted share not shown).
test_that("importances use the training seed, and the weighted share is shown", {
  ds <- review_frame()
  shiny::testServer(mod_model_server,
                    args = list(dataset_clean = shiny::reactive(ds),
                                recipe = shiny::reactive(default_recipe())), {
    session$setInputs(target = "y", positive = "oui", predictors = c("x", "noise", "g"),
                      sample_size = 900, group = NO_GROUP, mode = "stratified", test_prop = 0.2,
                      valid_prop = 0.25, models = "glm", balance = "weights",
                      balance_ratio = 1, seed = 1, importance_size = 150, importance_reps = 1,
                      train = 1)
    expect_match(output$distribution, "50,0 % des poids")
    session$setInputs(seed = 7, evaluate = 1)
    expected <- compute_importances(trained(), size = 150, n_rep = 1, seed = 1)
    expect_equal(importances()$permutation, expected$permutation)
  })
})

# Found in review: under resampling the reference level of a
# factor can differ between the weighted model and the unweighted refit, and
# the odds ratio of a row then refers to another contrast than its beta.
test_that("odds ratios use the same reference levels as the ranked coefficients", {
  withr::local_seed(1)
  n <- 3000
  y <- stats::rbinom(n, 1, 0.2)
  # Level A is the mode among negatives, B among positives: undersampling
  # changes the most frequent level, hence the reference of fit_glm().
  g <- ifelse(y == 1, sample(c("A", "B", "C"), n, TRUE, c(0.2, 0.6, 0.2)),
              sample(c("A", "B", "C"), n, TRUE, c(0.5, 0.3, 0.2)))
  data <- data.frame(y = factor(ifelse(y == 1, "oui", "non")), g = factor(g), v = rnorm(n))
  ds <- make_dataset(data, c(y = "quali", g = "quali", v = "quanti"))
  s <- list(target = "y", positive = "oui", predictors = c("g", "v"), sample_size = n, group = NULL,
            mode = "stratified", test_prop = 0.2, valid_prop = 0.25, models = "glm", seed = 1,
            balance = "under", balance_ratio = 1)
  out <- train_pipeline(ds, default_recipe(), s)
  r <- out$results$glm
  li <- compute_importances(out, size = 200)$logistic
  expect_false(anyNA(li$odds_ratio))
  # Unweighted fit by hand, with the reference of the ranked model.
  x <- r$recipe$train
  x$g <- stats::relevel(factor(as.character(x$g)), ref = r$model$levels$g[1])
  x$..y <- out$split$train$y[r$recipe$kept_rows]
  by_hand <- stats::glm(..y ~ ., data = x, family = stats::binomial())
  for (term in li$term) {
    expect_equal(li$odds_ratio[li$term == term], exp(stats::coef(by_hand)[[term]]), info = term)
  }
})

# Found in review: a level separated only in the resampled
# training set gets a runaway coefficient that ranks high on |beta| x s, but
# the flag reads the standard error of the unweighted fit.
test_that("a coefficient that runs away in the ranked model is flagged", {
  withr::local_seed(38)
  n <- 800
  x <- data.frame(v = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE), levels = c("a", "b", "s")))
  y <- stats::rbinom(n, 1, stats::plogis(x$v))
  # Level "s": 6 positives and 4 negatives; the 4 negatives are the rows a
  # resampling removed, so "s" is separated in the ranked model only.
  x$g[1:10] <- "s"
  y[1:10] <- c(rep(1, 6), rep(0, 4))
  kept <- setdiff(seq_len(n), 7:10)
  ranked <- suppressWarnings(fit_model("glm", x[kept, ], y[kept]))
  unweighted <- suppressWarnings(fit_model("glm", x, y))
  table <- logistic_table(logistic_importance(ranked, x, unweighted))
  expect_equal(table$Remarque[table$Terme == "gs"], "séparation probable")
  expect_equal(table$Remarque[table$Terme == "v"], "")
})

test_that("a numeric column on a tiny scale is not flagged as separated", {
  withr::local_seed(39)
  n <- 800
  x <- data.frame(v = rnorm(n, sd = 0.002))
  y <- stats::rbinom(n, 1, 0.3)
  li <- logistic_importance(fit_model("glm", x, y), x)
  expect_gt(li$se[li$term == "v"], SEPARATION_SE)  # the se follows the scale of v
  expect_false(li$separation[li$term == "v"])
})
