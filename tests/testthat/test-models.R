simulated <- function(n = 400, seed = 1) {
  withr::local_seed(seed)
  x <- rnorm(n)
  g <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  y <- rbinom(n, 1, stats::plogis(1.5 * x + (g == "b")))
  list(x = data.frame(x = x, g = g), y = y)
}

test_that("probabilities are in [0, 1], one per test row, for every model", {
  train <- simulated(seed = 1)
  test <- simulated(n = 137, seed = 2)
  for (type in names(MODEL_LABELS)) {
    model <- fit_model(type, train$x, train$y)
    prob <- predict_proba(model, test$x)
    expect_length(prob, 137L)
    expect_true(all(prob >= 0 & prob <= 1))
    expect_false(anyNA(prob))
  }
})

test_that("the coefficient of x is positive, and negative with the other level as positive", {
  d <- simulated(seed = 3)
  target <- factor(ifelse(d$y == 1, "haut", "bas"))
  for (positive in c("haut", "bas")) {
    y <- as.integer(as.character(target) == positive)
    model <- fit_model("glm", d$x, y)
    slope <- stats::coef(model$fit)[["x"]]
    if (positive == "haut") expect_gt(slope, 0) else expect_lt(slope, 0)
  }
})

test_that("a constant variable or a one-level factor does not break training", {
  d <- simulated(seed = 4)
  x <- cbind(d$x, k = 0.1, one = factor("seul"), empty_level = factor("u", levels = c("u", "v")))
  for (type in names(MODEL_LABELS)) {
    model <- fit_model(type, x, d$y)
    expect_setequal(model$dropped, c("k", "one", "empty_level"))
    expect_match(model$messages[1], "retirées")
    expect_length(predict_proba(model, x), nrow(x))
  }
})

test_that("ranger with 'order' separates a nominal factor that 'ignore' cannot split in one cut", {
  # Levels a, b, c, d (50 rows each); a and c are positive. With "order", levels
  # are sorted by their share of positives, so one cut separates {a, c} from
  # {b, d}. With "ignore", the alphabetical order a < b < c < d only allows
  # a | bcd, ab | cd or abc | d: the best stump misclassifies one level, 25 %.
  g <- factor(rep(c("a", "b", "c", "d"), each = 50))
  y <- as.integer(g %in% c("a", "c"))
  x <- data.frame(g = g)
  model <- fit_model("ranger", x, y, num.trees = 1, max.depth = 1, seed = 1)
  predicted <- as.integer(predict_proba(model, x) > 0.5)
  expect_equal(mean(predicted != y), 0)
  ignore <- ranger::ranger(x = x, y = factor(y, levels = c(0, 1)), num.trees = 1, max.depth = 1,
                           seed = 1, probability = TRUE, respect.unordered.factors = "ignore",
                           num.threads = 1)
  predicted_ignore <- as.integer(stats::predict(ignore, data = x)$predictions[, "1"] > 0.5)
  expect_equal(mean(predicted_ignore != y), 0.25)
})

test_that("the logistic regression takes the most frequent level as reference", {
  d <- simulated(seed = 5)
  d$x$g <- factor(rep(c("z", "z", "a", "m"), length.out = nrow(d$x)))
  model <- fit_model("glm", d$x, d$y)
  expect_equal(model$levels$g[1], "z")
  expect_false("gz" %in% names(stats::coef(model$fit)))
})

test_that("non-integer weights use quasibinomial with the same coefficients", {
  d <- simulated(seed = 6)
  w <- observation_weights(d$y)
  weighted <- fit_model("glm", d$x, d$y, weights = w)
  expect_equal(weighted$fit$family$family, "quasibinomial")
  reference <- suppressWarnings(stats::glm(d$y ~ x + g, data = d$x, family = stats::binomial(),
                                           weights = w))
  expect_equal(unname(stats::coef(weighted$fit)), unname(stats::coef(reference)))
  expect_length(weighted$messages, 0)
  # ranger accepts the same per-row weights.
  expect_length(predict_proba(fit_model("ranger", d$x, d$y, weights = w), d$x), nrow(d$x))
})

test_that("missing values after the recipe, other classes and one-class targets are refused", {
  d <- simulated(seed = 7)
  with_na <- d$x
  with_na$x[3] <- NA
  expect_error(fit_model("glm", with_na, d$y), "imputation")
  expect_error(fit_model("glm", cbind(d$x, code = "c1"), d$y), "ni numériques")
  expect_error(fit_model("ranger", d$x, rep(1, nrow(d$x))), "une classe")
  expect_error(fit_model("svm", d$x, d$y), "inconnu")
})

test_that("an unseen level at prediction time gives a clear message for the logistic regression and the tree", {
  d <- simulated(seed = 8)
  new <- d$x[1:2, ]
  new$g <- factor(c("a", "zzz"))
  for (type in c("glm", "rpart")) {
    expect_error(predict_proba(fit_model(type, d$x, d$y), new), "modalité inconnue")
  }
})

test_that("the whole pipeline runs on students.csv with Sex as target", {
  path <- test_path("fixtures", "course", "students.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  data <- read_table_file(path, na_codes = c("NA", ""))
  ds <- make_dataset(data, detect_types(data))
  recipe <- utils::modifyList(default_recipe(), list(
    impute = list(quanti = "median", quali = "mode"), rare = list(threshold = 0.05),
    scale = list(method = "standard"), dummy = list(method = "one_hot")))
  settings <- list(target = "Sex", positive = "female",
                   predictors = c("Height", "Sleep", "Level", "Brothers", "MilesHome"),
                   sample_size = 1000, group = NULL, mode = "stratified", test_prop = 0.2,
                   valid_prop = 0.25, models = c("glm", "ranger"), seed = 1)
  out <- train_pipeline(ds, recipe, settings)
  expect_equal(sum(out$distribution$n), 48L)
  expect_equal(names(out$results), c("glm", "ranger"))
  for (r in out$results) {
    expect_length(r$prob_valid, out$distribution$n[2])
    expect_equal(sum(r$confusion), out$distribution$n[2])
  }
  # Height separates the sexes: positive female means a negative slope.
  expect_lt(stats::coef(out$results$glm$model$fit)[["Height"]], 0)
  expect_match(out$results$glm$messages, "k − 1", all = FALSE)
})

test_that("the pipeline codes the chosen level as 1 and warns about cut groups", {
  withr::local_seed(9)
  n <- 600
  data <- data.frame(y = factor(sample(c("oui", "non"), n, TRUE)), x = rnorm(n),
                     year = rep(2019:2024, length.out = n), acc = rep(1:200, each = 3))
  ds <- make_dataset(data, c(y = "quali", x = "quanti", year = "quanti", acc = "id"))
  settings <- list(target = "y", positive = "oui", predictors = c("x"), sample_size = n,
                   group = "acc", mode = "variable", by = "year", test_values = "2024",
                   test_prop = 0.2, valid_prop = 0.25, models = "glm", seed = 1)
  out <- train_pipeline(ds, default_recipe(), settings)
  all_y <- c(out$split$train$y, out$split$valid$y, out$split$test$y)
  expect_equal(sum(all_y), sum(data$y == "oui"))
  # Each accident spans three consecutive years, so some cross 2024.
  expect_match(out$messages, "coupés", all = FALSE)
})

test_that("the modelling module trains on the button and shows the validation metrics", {
  withr::local_seed(10)
  n <- 300
  data <- data.frame(y = factor(sample(c("non", "oui"), n, TRUE, prob = c(0.7, 0.3))),
                     x = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  ds <- make_dataset(data, c(y = "quali", x = "quanti", g = "quali"))
  shiny::testServer(mod_model_server,
                    args = list(dataset_clean = shiny::reactive(ds),
                                recipe = shiny::reactive(default_recipe())), {
    session$setInputs(target = "y", positive = "oui", predictors = c("x", "g"), sample_size = n,
                      group = NO_GROUP, mode = "stratified", test_prop = 0.2, valid_prop = 0.25,
                      models = c("glm", "ranger"), seed = 1)
    expect_null(trained())
    session$setInputs(train = 1)
    out <- trained()
    expect_equal(names(out$results), c("glm", "ranger"))
    expect_match(output$distribution, "Validation")
    expect_match(output$metrics, "Forêt aléatoire")
    expect_match(output$confusions$html, "Matrice de confusion")
    # Invalid settings give a message, not an error.
    session$setInputs(models = character(0), train = 2)
    expect_null(trained())
  })
})

test_that("results are cleared when the data or the recipe change", {
  withr::local_seed(11)
  n <- 200
  make <- function() make_dataset(
    data.frame(y = factor(sample(c("non", "oui"), n, TRUE)), x = rnorm(n)),
    c(y = "quali", x = "quanti"))
  ds <- shiny::reactiveVal(make())
  rc <- shiny::reactiveVal(default_recipe())
  shiny::testServer(mod_model_server, args = list(dataset_clean = ds, recipe = rc), {
    session$setInputs(target = "y", positive = "oui", predictors = "x", sample_size = n,
                      group = NO_GROUP, mode = "stratified", models = "glm", seed = 1, train = 1)
    expect_false(is.null(trained()))
    ds(make())
    session$flushReact()
    expect_null(trained())
    session$setInputs(train = 2)
    expect_false(is.null(trained()))
    rc(utils::modifyList(default_recipe(), list(scale = list(method = "standard"))))
    session$flushReact()
    expect_null(trained())
  })
})

test_that("a file under 50 rows can be modelled with its default sample size", {
  ds <- make_dataset(data.frame(y = factor(rep(c("a", "b"), 24)), x = 1:48),
                     c(y = "quali", x = "quanti"))
  s <- list(target = "y", positive = "b", predictors = "x", sample_size = 48, group = NULL,
            mode = "stratified", models = "glm", seed = 1)
  expect_null(settings_problem(s, ds))
  expect_match(settings_problem(utils::modifyList(s, list(sample_size = 20)), ds), "48")
})

# --- Pipeline properties (tests added after mutation testing) ------------------

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

test_that("the recipe sees the training set only, and the metrics use the validation set", {
  ds <- pipeline_data()
  recipe <- utils::modifyList(default_recipe(), list(
    impute = list(quanti = "median", quali = "mode"), scale = list(method = "standard"),
    dummy = list(method = "k_minus_1")))
  out <- train_pipeline(ds, recipe, pipeline_settings())
  types <- ds$var_types[c("x", "z", "g")]
  for (type in names(out$results)) {
    r <- out$results[[type]]
    # Recipe parameters equal those learned on the training rows alone.
    expect_equal(r$recipe$params, fit_recipe(recipe, out$split$train$x, types)$params)
    # Validation probabilities, threshold and confusion matrix come from the validation set.
    expect_equal(r$prob_valid, predict_proba(r$model, apply_recipe(r$recipe, out$split$valid$x)))
    expect_equal(r$threshold, best_f1_threshold(out$split$valid$y, r$prob_valid))
    expect_equal(r$confusion, confusion(out$split$valid$y, r$prob_valid, r$threshold))
    expect_equal(r$roc$auc, roc_curve(out$split$valid$y, r$prob_valid)$auc)
  }
})

test_that("one-hot is replaced by k - 1 dummies for the logistic regression only", {
  ds <- pipeline_data()
  recipe <- utils::modifyList(default_recipe(), list(dummy = list(method = "one_hot")))
  out <- train_pipeline(ds, recipe, pipeline_settings())
  expect_equal(out$results$glm$recipe$recipe$dummy$method, "k_minus_1")
  expect_equal(out$results$ranger$recipe$recipe$dummy$method, "one_hot")
  expect_false(anyNA(stats::coef(out$results$glm$model$fit)))
  expect_length(grep("^g_", out$results$glm$model$columns), 2L)
  expect_length(grep("^g_", out$results$ranger$model$columns), 3L)
})

test_that("rows without target are removed and the target is never a predictor", {
  ds <- pipeline_data()
  ds$data$y[c(1, 5, 9)] <- NA
  out <- train_pipeline(ds, default_recipe(), pipeline_settings(predictors = c("x", "y", "g")))
  expect_match(out$messages, "3 lignes sans cible", all = FALSE)
  expect_equal(sum(out$distribution$n), nrow(ds$data) - 3L)
  expect_false("y" %in% names(out$split$train$x))
})

test_that("the same seed gives the same models, another seed other ones", {
  ds <- pipeline_data()
  a <- train_pipeline(ds, default_recipe(), pipeline_settings(models = "ranger"))
  b <- train_pipeline(ds, default_recipe(), pipeline_settings(models = "ranger"))
  c <- train_pipeline(ds, default_recipe(), pipeline_settings(models = "ranger", seed = 2))
  expect_identical(a$results$ranger$prob_valid, b$results$ranger$prob_valid)
  expect_false(identical(a$split$valid$rows, c$split$valid$rows))
})

test_that("outlier removal keeps the target aligned with the remaining training rows", {
  ds <- pipeline_data()
  ds$data$z[1:6] <- 1e4
  recipe <- utils::modifyList(default_recipe(), list(outliers = list(action = "remove")))
  out <- train_pipeline(ds, recipe, pipeline_settings(models = "glm"))
  r <- out$results$glm
  expect_lt(nrow(r$recipe$train), length(out$split$train$y))
  expect_equal(length(r$model$fit$y), nrow(r$recipe$train))
  expect_equal(unname(r$model$fit$y), out$split$train$y[r$recipe$kept_rows])
})

test_that("a sample by group keeps every group whole", {
  ds <- pipeline_data()
  # 301 rows asked, each accident has 3 rows: a grouped sample takes 101 whole
  # accidents (303 rows); a sample by rows would cut some accidents.
  out <- train_pipeline(ds, default_recipe(), pipeline_settings(sample_size = 301, models = "glm"))
  expect_length(out$sample_rows, 303L)
  expect_true(all(table(ds$data$acc[out$sample_rows]) == 3))
})

test_that("a stratified split with a group id is refused, as it would cut groups", {
  ds <- pipeline_data()
  s <- pipeline_settings(mode = "stratified")
  expect_match(settings_problem(s, ds), "groupé")
  expect_null(settings_problem(pipeline_settings(), ds))
  expect_match(settings_problem(pipeline_settings(positive = "peut-être"), ds), "n'existe pas")
})

test_that("a predictor named weights is not taken as glm weights", {
  withr::local_seed(13)
  n <- 300
  x <- data.frame(x = rnorm(n), weights = runif(n, 10, 20))
  y <- stats::rbinom(n, 1, stats::plogis(x$x))
  model <- fit_model("glm", x, y)
  expect_equal(unname(model$fit$prior.weights), rep(1, n))
  expect_equal(model$fit$family$family, "binomial")
  expect_true("weights" %in% names(stats::coef(model$fit)))
  w <- observation_weights(y)
  expect_equal(unname(fit_model("glm", x, y, weights = w)$fit$prior.weights), w)
})

test_that("no informative column and missing values at prediction give French messages", {
  expect_error(fit_model("ranger", data.frame(k = rep(1, 20)), rep(0:1, 10)), "aucune variable")
  model <- fit_model("glm", data.frame(x = 1:20 + rnorm(20)), rep(0:1, 10))
  expect_error(predict_proba(model, data.frame(x = c(1, NA))), "imputation")
})

# --- E11a: tree, reduced QR, thresholds, test on demand, leak alert -----------------

test_that("fitting and predicting with any model leaves the global random state untouched", {
  # The server shares one random state between sessions; rpart draws from it
  # for its internal cross-validation unless xval = 0.
  d <- simulated(seed = 14)
  set.seed(99)
  state <- .Random.seed
  for (type in names(MODEL_LABELS)) {
    model <- fit_model(type, d$x, d$y)
    invisible(predict_proba(model, d$x))
    expect_identical(.Random.seed, state, info = type)
  }
})

test_that("the reduced QR keeps predictions and Wald intervals of the full logistic regression", {
  withr::local_seed(15)
  n <- 800
  x <- data.frame(a = rnorm(n), f = factor(sample(c("p", "q", "r"), n, TRUE)),
                  b = runif(n))
  x$a2 <- 2 * x$a + 1  # collinear with a: one coefficient is not estimable
  x$g <- factor(ifelse(x$f == "p", "P", "autre"))  # nested in f
  y <- stats::rbinom(n, 1, stats::plogis(x$a + (x$f == "q")))
  new <- x[sample(n, 200), ]
  for (w in list(NULL, stats::runif(n, 0.2, 3))) {
    model <- fit_model("glm", x, y, weights = w)
    expect_true(anyNA(stats::coef(model$fit)))
    p <- ncol(model$fit$qr$qr)
    expect_equal(nrow(model$fit$qr$qr), p)
    # Reference: the same fit by glm() with the same reference levels, full QR kept.
    data <- x
    for (v in names(model$levels)) data[[v]] <- factor(data[[v]], levels = model$levels[[v]])
    data$..y <- y
    ref_w <- if (is.null(w)) rep(1, n) else w
    family <- if (is.null(w)) stats::binomial() else stats::quasibinomial()
    reference <- stats::glm(..y ~ ., data = data, family = family, weights = ref_w)
    expect_gt(nrow(reference$qr$qr), p)
    newdata <- new
    for (v in names(model$levels)) newdata[[v]] <- factor(newdata[[v]], levels = model$levels[[v]])
    expected <- unname(suppressWarnings(stats::predict(reference, newdata = newdata, type = "response")))
    expect_identical(suppressWarnings(predict_proba(model, new)), expected)
    expect_equal(stats::confint.default(model$fit), stats::confint.default(reference))
  }
})

test_that("the thresholds do not depend on the test labels", {
  # Split by variable: the test rows are the 2024 ones whatever their labels,
  # and the validation set is drawn from the training rows only.
  ds <- pipeline_data()
  s <- pipeline_settings(group = NULL, mode = "variable", by = "year", test_values = "2024")
  a <- train_pipeline(ds, default_recipe(), s)
  flipped <- ds
  in_test <- ds$data$year == 2024
  flipped$data$y[in_test] <- ifelse(ds$data$y[in_test] == "oui", "non", "oui")
  b <- train_pipeline(flipped, default_recipe(), s)
  for (type in names(a$results)) {
    expect_identical(a$results[[type]]$threshold, b$results[[type]]$threshold)
    expect_identical(a$results[[type]]$prob_valid, b$results[[type]]$prob_valid)
  }
  thr <- vapply(a$results, `[[`, numeric(1), "threshold")
  expect_false(identical(evaluate_test(a, thr)$results$glm$metrics,
                         evaluate_test(b, thr)$results$glm$metrics))
})

test_that("the test evaluation uses the fixed thresholds and the fitted recipes only", {
  ds <- pipeline_data()
  recipe <- utils::modifyList(default_recipe(), list(scale = list(method = "standard")))
  out <- train_pipeline(ds, recipe, pipeline_settings())
  thr <- c(glm = 0.3, ranger = 0.6)
  res <- evaluate_test(out, thr)
  for (type in names(thr)) {
    r <- out$results[[type]]
    prob <- predict_proba(r$model, apply_recipe(r$recipe, out$split$test$x))
    expect_equal(res$results[[type]]$threshold, thr[[type]])
    expect_equal(res$results[[type]]$confusion, confusion(out$split$test$y, prob, thr[[type]]))
    expect_equal(res$results[[type]]$roc$auc, roc_curve(out$split$test$y, prob)$auc)
  }
  expect_equal(res$naive$all_positive$metrics[["precision"]], mean(out$split$test$y))
  expect_error(evaluate_test(out, c(glm = 0.3)), "seuil manquant")
})

test_that("a predictor copying the target raises the leak alert in the pipeline", {
  ds <- pipeline_data()
  ds$data$copy <- factor(as.character(ds$data$y))
  ds$var_types[["copy"]] <- "quali"
  out <- train_pipeline(ds, default_recipe(), pipeline_settings(predictors = c("x", "g", "copy")))
  expect_equal(out$leak, "copy")
  expect_equal(out$univariate_auc[["copy"]], 1)
  expect_match(out$messages, "Liaison quasi parfaite.*copy", all = FALSE)
  clean <- train_pipeline(ds, default_recipe(), pipeline_settings())
  expect_length(clean$leak, 0)
})

test_that("the test set is evaluated only on the button, with frozen thresholds", {
  withr::local_seed(16)
  n <- 400
  data <- data.frame(y = factor(sample(c("non", "oui"), n, TRUE, prob = c(0.7, 0.3))),
                     x = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  data$x <- data$x + (data$y == "oui")
  ds <- make_dataset(data, c(y = "quali", x = "quanti", g = "quali"))
  calls <- 0
  shiny::testServer(mod_model_server,
                    args = list(dataset_clean = shiny::reactive(ds),
                                recipe = shiny::reactive(default_recipe())), {
    session$setInputs(target = "y", positive = "oui", predictors = c("x", "g"), sample_size = n,
                      group = NO_GROUP, mode = "stratified", test_prop = 0.2, valid_prop = 0.25,
                      models = c("glm", "rpart"), seed = 1, train = 1)
    out <- trained()
    expect_false(is.null(out))
    expect_null(tested())
    expect_match(output$metrics, "Référence : tout positif")
    # Thresholds start at the maximal F1 of the validation set.
    expect_equal(thresholds()[["glm"]], out$results$glm$threshold)
    # Moving a slider changes the validation metrics.
    before <- output$metrics
    session$setInputs(threshold_glm_1 = 0.9)
    expect_equal(thresholds()[["glm"]], 0.9)
    expect_false(identical(output$metrics, before))
    session$setInputs(evaluate = 1)
    res <- tested()
    expect_equal(res$results$glm$threshold, 0.9)
    expect_match(output$test_info, "0,900")
    frozen <- output$test_metrics
    # Moving the slider after the evaluation leaves the test results unchanged.
    session$setInputs(threshold_glm_1 = 0.2)
    expect_identical(tested(), res)
    expect_identical(output$test_metrics, frozen)
    # A new training clears them, and its sliders get new ids.
    session$setInputs(train = 2)
    expect_null(tested())
    expect_equal(thresholds()[["glm"]], trained()$results$glm$threshold)
  })
})
