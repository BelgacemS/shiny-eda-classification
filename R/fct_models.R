# Models with a common interface (PLAN.md, step E10b), and the training
# pipeline of the modelling tab.
#
# fit_model(type, x, y, weights, ...) learns a model on predictors x (numeric
# columns and factors, after the recipe) and a 0/1 target y; predict_proba()
# gives the probability of class 1. Columns without information in training
# (constant, or a factor with a single level) are dropped with a message.

MODEL_LABELS <- c(glm = "Régression logistique", ranger = "Forêt aléatoire",
                  rpart = "Arbre de décision")

# Trees of the forest in the app: enough for stable probabilities on samples
# of a few tens of thousands of rows, in a few seconds.
RANGER_TREES <- 300

fit_model <- function(type, x, y, weights = NULL, ...) {
  if (!type %in% names(MODEL_LABELS)) {
    stop("modèle inconnu : ", type, call. = FALSE)
  }
  x <- as.data.frame(x)
  if (nrow(x) != length(y) || anyNA(y) || !all(y %in% c(0, 1))) {
    stop("la cible doit valoir 0 ou 1, une valeur par ligne de x.", call. = FALSE)
  }
  bad_class <- names(x)[!vapply(x, function(v) is.numeric(v) || is.factor(v) || is.logical(v),
                                logical(1))]
  if (length(bad_class) > 0) {
    stop("variables ni numériques ni qualitatives : ", paste(bad_class, collapse = ", "),
         call. = FALSE)
  }
  with_na <- names(x)[vapply(x, anyNA, logical(1))]
  if (length(with_na) > 0) {
    stop("valeurs manquantes après la recette dans : ", paste(with_na, collapse = ", "),
         " ; choisir une imputation dans l'onglet Prétraitement.", call. = FALSE)
  }
  # Levels without rows: resampling the training set after the recipe can
  # remove a rare level that the validation and test sets still have.
  absent <- lapply(x, function(v) if (is.factor(v)) setdiff(levels(v), as.character(v)) else character(0))
  x[] <- lapply(x, function(v) if (is.factor(v)) droplevels(v) else if (is.logical(v)) as.numeric(v) else v)
  dropped <- names(x)[vapply(x, uninformative, logical(1))]
  x <- x[setdiff(names(x), dropped)]
  messages <- if (length(dropped) > 0) {
    paste0("variables sans information dans l'entraînement, retirées : ",
           paste(dropped, collapse = ", "))
  } else {
    character(0)
  }
  # Such a level is predicted as the most frequent training level, as the
  # recipe does for an unseen level when it has no "Autre" level; a level
  # absent from the data stays an error.
  fallback <- list()
  for (v in intersect(names(x)[vapply(x, is.factor, logical(1))], names(absent))) {
    if (length(absent[[v]]) > 0) {
      counts <- table(x[[v]])
      fallback[[v]] <- list(levels = absent[[v]], to = names(counts)[which.max(counts)])
    }
  }
  if (length(fallback) > 0) {
    messages <- c(messages, paste0(
      "modalités sans ligne dans l'entraînement (après rééquilibrage), prédites comme la modalité ",
      "la plus fréquente : ", paste(vapply(names(fallback), function(v) {
        sprintf("%s (%s)", v, paste(fallback[[v]]$levels, collapse = ", "))
      }, character(1)), collapse = " ; ")))
  }
  if (length(unique(y)) < 2) {
    stop("l'entraînement ne contient qu'une classe de la cible.", call. = FALSE)
  }
  if (ncol(x) == 0) {
    stop("aucune variable explicative informative dans l'entraînement.", call. = FALSE)
  }
  fit <- switch(type, glm = fit_glm(x, y, weights, ...), ranger = fit_ranger(x, y, weights, ...),
                rpart = fit_rpart(x, y, weights, ...))
  structure(
    list(type = type, fit = fit$model, columns = names(x), levels = fit$levels,
         fallback = fallback, dropped = dropped, messages = c(messages, fit$messages)),
    class = "fitted_model"
  )
}

# A column is uninformative when it is constant in training (a factor with a
# single level, or a numeric column with a single value).
uninformative <- function(v) {
  if (is.factor(v)) nlevels(v) < 2 else is_constant(v)
}

# Logistic regression. Each factor takes its most frequent training level as
# reference (the alphabetical order depends on the locale), unless reference
# (a named list, variable -> level) gives another one: a refit can then keep
# the references of another fit. quasibinomial when the weights are not
# integers: same coefficients, without the warning. Other arguments in ... are
# ignored (the seed of the pipeline: the fit is deterministic).
fit_glm <- function(x, y, weights, reference = NULL, ...) {
  factors <- names(x)[vapply(x, is.factor, logical(1))]
  levels <- lapply(stats::setNames(factors, factors), function(v) {
    counts <- table(x[[v]])
    first <- reference[[v]]
    if (is.null(first) || !first %in% levels(x[[v]])) first <- names(counts)[which.max(counts)]
    c(first, setdiff(levels(x[[v]]), first))
  })
  for (v in factors) x[[v]] <- factor(x[[v]], levels = levels[[v]])
  # The target goes in a reserved column; the weights stay out of the data, in
  # ..w of this function: glm() looks for `weights` in the data first, so a
  # predictor named "weights" would otherwise become the weights.
  data <- x
  data$..y <- y
  ..w <- if (is.null(weights)) rep(1, length(y)) else weights
  integer_weights <- all(..w == round(..w))
  family <- if (integer_weights) stats::binomial() else stats::quasibinomial()
  warnings <- character(0)
  model <- withCallingHandlers(
    stats::glm(..y ~ ., data = data, family = family, weights = ..w),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  messages <- if (length(warnings) > 0) {
    paste("régression logistique :", unique(warnings))
  } else {
    character(0)
  }
  # predict() reads only the pivot of the QR decomposition and, on a
  # rank-deficient fit, qr.R(), as do summary() and confint.default(): they
  # use the first p rows of the n x p matrix, the largest part of the object.
  # hatvalues(), lm.influence() and predict(se.fit = TRUE) without new data no
  # longer work (none is used). Done on the variable `model` of this frame,
  # which the formula environment holds: a copy truncated elsewhere would
  # leave the full matrix alive.
  model$qr$qr <- model$qr$qr[seq_len(min(dim(model$qr$qr))), , drop = FALSE]
  aliased <- names(stats::coef(model))[is.na(stats::coef(model))]
  if (length(aliased) > 0) {
    messages <- c(messages, paste0("régression logistique : coefficients non estimables ",
                                   "(variables colinéaires) : ", paste(aliased, collapse = ", ")))
  }
  list(model = model, levels = levels, messages = messages)
}

# Random forest of probabilities. respect.unordered.factors = "order": the
# default "ignore" treats a nominal factor as ordered by its level order
# (Wright and König, 2019). Per-row weights go to case.weights, the probability
# of drawing each row in the bootstrap. Arguments in ... go to ranger().
fit_ranger <- function(x, y, weights, ...) {
  args <- utils::modifyList(
    list(num.trees = RANGER_TREES, probability = TRUE, respect.unordered.factors = "order",
         seed = 1, num.threads = 1),
    list(...)
  )
  args <- c(list(x = x, y = factor(y, levels = c(0, 1)), case.weights = weights), args)
  list(model = do.call(ranger::ranger, args), levels = NULL, messages = character(0))
}

# Levels without rows in training replaced by the level of model$fallback.
apply_fallback <- function(model, newx) {
  for (v in names(model$fallback)) {
    fb <- model$fallback[[v]]
    values <- as.character(newx[[v]])
    values[values %in% fb$levels] <- fb$to
    newx[[v]] <- factor(values, levels = union(setdiff(levels(newx[[v]]), fb$levels), fb$to))
  }
  newx
}

# Classification tree. xval = 0: the internal cross-validation of rpart draws
# from the global random state, shared by all sessions of the server, and is
# not needed with a fixed cp. Weights and target stay out of the predictor
# names, as in fit_glm(). Arguments in ... go to rpart.control(), which drops
# those it does not know (the seed of the pipeline: the fit is deterministic).
fit_rpart <- function(x, y, weights, ...) {
  factors <- names(x)[vapply(x, is.factor, logical(1))]
  levels <- lapply(stats::setNames(factors, factors), function(v) levels(x[[v]]))
  data <- x
  data$..y <- factor(y, levels = c(0, 1))
  ..w <- if (is.null(weights)) rep(1, length(y)) else weights
  control <- do.call(rpart::rpart.control, utils::modifyList(list(cp = 0.01, xval = 0), list(...)))
  model <- rpart::rpart(..y ~ ., data = data, method = "class", weights = ..w, control = control)
  list(model = model, levels = levels, messages = character(0))
}

# Probability of class 1 for each row of newx. The only attribute is
# non_estimable: rows whose logistic prediction is not estimable (integer(0)
# for the forest and the tree, and for a logistic regression of full rank).
predict_proba <- function(model, newx) {
  stopifnot(inherits(model, "fitted_model"))
  newx <- as.data.frame(newx)
  missing <- setdiff(model$columns, names(newx))
  if (length(missing) > 0) {
    stop("colonnes absentes : ", paste(missing, collapse = ", "), call. = FALSE)
  }
  newx <- newx[model$columns]
  newx[] <- lapply(newx, function(v) if (is.logical(v)) as.numeric(v) else v)
  newx <- apply_fallback(model, newx)
  with_na <- names(newx)[vapply(newx, anyNA, logical(1))]
  if (length(with_na) > 0) {
    stop("valeurs manquantes après la recette dans : ", paste(with_na, collapse = ", "),
         " ; choisir une imputation dans l'onglet Prétraitement.", call. = FALSE)
  }
  for (v in names(model$levels)) {
    unseen <- setdiff(unique(as.character(newx[[v]])), c(model$levels[[v]], NA))
    if (length(unseen) > 0) {
      stop("modalité inconnue de l'entraînement dans ", v, " : ", paste(unseen, collapse = ", "),
           call. = FALSE)
    }
    newx[[v]] <- factor(as.character(newx[[v]]), levels = model$levels[[v]])
  }
  if (model$type == "glm") {
    # On a rank-deficient fit, a new row outside the span of the training
    # design is not estimable: its prediction depends on the arbitrary pivot of
    # the QR decomposition. predict.lm(rankdeficient = "non-estim") gives the
    # same linear predictor as predict.glm() without the warning and lists
    # those rows; predict.glm() does not pass rankdeficient on.
    link <- stats::predict.lm(model$fit, newdata = newx, rankdeficient = "non-estim")
    rows <- attr(link, "non-estim")
    return(structure(model$fit$family$linkinv(as.vector(link)),
                     non_estimable = if (is.null(rows)) integer(0) else as.integer(unname(rows))))
  }
  prob <- if (model$type == "rpart") {
    stats::predict(model$fit, newdata = newx, type = "prob")[, "1"]
  } else {
    # A fixed seed: predict.ranger() otherwise draws one from the global random
    # state, shared by all sessions of the server.
    stats::predict(model$fit, data = newx, num.threads = 1, seed = 1)$predictions[, "1"]
  }
  structure(as.vector(prob), non_estimable = integer(0))
}

# Message on the logistic predictions that are not estimable, NULL when none.
non_estimable_message <- function(prob, part) {
  n <- length(attr(prob, "non_estimable"))
  if (n == 0) return(NULL)
  sprintf(paste0("régression logistique : %d ligne%s %s sur %d non estimable%s : leur probabilité ",
                 "dépend d'un choix arbitraire du calcul (coefficients non estimables). Regrouper ",
                 "les modalités rares ou retirer les variables redondantes."),
          n, if (n > 1) "s" else "", part, length(prob), if (n > 1) "s" else "")
}

# --- Pipeline of the modelling tab ------------------------------------------

# settings: target, positive (level of the target coded 1), predictors,
# sample_size, group (column name or NULL), mode, test_prop, valid_prop,
# by (column name), test_values, models (types), seed; balance (a method of
# BALANCE_METHODS, "none" by default) and balance_ratio (resampling ratio, 1
# by default), applied to the training set after the recipe.
# Steps: rows without target removed, target coded 0/1, sample, split, recipe
# fitted on the training set (k - 1 dummies imposed for the logistic
# regression), models; on the validation set: ROC, AUC, threshold of maximal
# F1 and metrics at that threshold; univariate AUC of each original predictor
# (leak alert). The test set is not predicted here: evaluate_test() does it on
# demand, with thresholds fixed beforehand. timings: seconds per part.
train_pipeline <- function(dataset, recipe, settings) {
  data <- dataset$data
  types <- dataset$var_types
  target <- settings$target
  predictors <- setdiff(settings$predictors, target)
  if (length(predictors) == 0) {
    stop("choisir au moins une variable explicative.", call. = FALSE)
  }
  messages <- character(0)
  has_target <- !is.na(data[[target]])
  if (!all(has_target)) {
    messages <- c(messages, sprintf("%d lignes sans cible retirées.", sum(!has_target)))
    data <- data[has_target, , drop = FALSE]
  }
  y <- as.integer(as.character(data[[target]]) == settings$positive)

  group <- if (!is.null(settings$group) && settings$group %in% names(data)) data[[settings$group]]
  rows <- sample_rows(nrow(data), settings$sample_size, groups = group, seed = settings$seed)
  data <- data[rows, , drop = FALSE]
  y <- y[rows]
  group <- group[rows]
  by <- if (settings$mode == "variable") data[[settings$by]]
  split <- split_data(data[predictors], y, settings$mode, test_prop = settings$test_prop,
                      valid_prop = settings$valid_prop, groups = group, by = by,
                      test_values = settings$test_values, seed = settings$seed)
  crossing <- attr(split, "crossing_groups")
  if (!is.null(crossing) && crossing > 0) {
    messages <- c(messages, sprintf(
      "%d groupes ont des lignes de part et d'autre des valeurs de test : ils sont coupés entre l'entraînement et le test.",
      crossing))
  }

  timings <- numeric(0)
  results <- lapply(stats::setNames(settings$models, settings$models), function(type) {
    started <- proc.time()[["elapsed"]]
    model_recipe <- recipe
    notes <- character(0)
    if (type == "glm" && identical(recipe$dummy$method, "one_hot")) {
      model_recipe$dummy$method <- "k_minus_1"
      notes <- "régression logistique : dummification en k − 1 colonnes au lieu de one-hot, colinéaire avec la constante."
    }
    fitted <- fit_recipe(model_recipe, split$train$x, types[predictors])
    y_train <- split$train$y[fitted$kept_rows]
    # Rebalancing touches the training set only: validation and test keep the
    # real class distribution.
    method <- settings$balance %||% "none"
    balanced <- balance_train(list(train = list(x = fitted$train, y = y_train)), method,
                              settings$balance_ratio %||% 1, settings$seed)$train
    model <- fit_model(type, balanced$x, balanced$y, weights = balanced$weights,
                       seed = settings$seed)
    x_valid <- apply_recipe(fitted, split$valid$x)
    prob <- predict_proba(model, x_valid)
    fitted_at <- proc.time()[["elapsed"]]
    threshold <- best_f1_threshold(split$valid$y, prob)
    at_threshold <- threshold_metrics(split$valid$y, prob, threshold)
    roc <- roc_curve(split$valid$y, prob)
    timings[[type]] <<- fitted_at - started
    timings[[paste0(type, "_roc_threshold")]] <<- proc.time()[["elapsed"]] - fitted_at
    w <- balanced$weights %||% rep(1, length(balanced$y))
    balance <- list(method = method, n = length(balanced$y), positives = sum(balanced$y),
                    share = sum(w * balanced$y) / sum(w))
    list(model = model, recipe = fitted, prob_valid = prob, threshold = threshold, balance = balance,
         confusion = at_threshold$confusion, metrics = at_threshold$metrics, roc = roc,
         messages = unique(c(notes, fitted$messages, model$messages,
                             non_estimable_message(prob, "de la validation"))))
  })

  # Leak alert on the original predictors, before the recipe and without weights.
  started <- proc.time()[["elapsed"]]
  uni_auc <- univariate_auc(split$train$x, split$train$y, split$valid$x, split$valid$y)
  timings[["leak_alert"]] <- proc.time()[["elapsed"]] - started
  leak <- names(uni_auc)[!is.na(uni_auc) & uni_auc > LEAK_AUC]
  if (length(leak) > 0) {
    messages <- c(messages, sprintf(paste0(
      "Liaison quasi parfaite avec la cible (AUC univariée sur la validation supérieure à %s) : %s. ",
      "Fuite probable : variable dérivée de la cible ou quasi identifiante, à retirer des variables explicatives."),
      format_number(LEAK_AUC, 2), paste(leak, collapse = ", ")))
  }

  # sample_rows: indices of the sample in the data with a target (the split's
  # rows index this sample).
  list(split = split, results = results, messages = messages, sample_rows = rows,
       distribution = target_distribution(split), target = target, positive = settings$positive,
       naive_valid = naive_metrics(split$valid$y), univariate_auc = uni_auc, leak = leak,
       seed = settings$seed, timings = timings)
}

# Test evaluation, on demand: each model predicts the test set through its
# fitted recipe and is scored at the threshold given in thresholds (named by
# model type, fixed before the call). Nothing is learned on the test set.
evaluate_test <- function(trained, thresholds) {
  y <- trained$split$test$y
  results <- lapply(stats::setNames(names(trained$results), names(trained$results)), function(type) {
    r <- trained$results[[type]]
    threshold <- if (type %in% names(thresholds)) thresholds[[type]] else NA_real_
    if (is.na(threshold)) {
      stop("seuil manquant pour ", MODEL_LABELS[[type]], call. = FALSE)
    }
    prob <- predict_proba(r$model, apply_recipe(r$recipe, trained$split$test$x))
    at_threshold <- threshold_metrics(y, prob, threshold)
    list(threshold = threshold, confusion = at_threshold$confusion,
         metrics = at_threshold$metrics, roc = roc_curve(y, prob),
         non_estimable = length(attr(prob, "non_estimable")),
         message = non_estimable_message(prob, "du test"))
  })
  list(results = results, naive = naive_metrics(y))
}

# Size and share of positives of each part.
target_distribution <- function(split) {
  labels <- c(train = "Entraînement", valid = "Validation", test = "Test")
  data.frame(
    part = unname(labels[names(split)]),
    n = vapply(split, function(p) length(p$y), integer(1)),
    positives = vapply(split, function(p) as.integer(sum(p$y)), integer(1)),
    share = vapply(split, function(p) mean(p$y), numeric(1)),
    row.names = NULL
  )
}
