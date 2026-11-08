# Models with a common interface (PLAN.md, step E10b), and the training
# pipeline of the modelling tab.
#
# fit_model(type, x, y, weights, ...) learns a model on predictors x (numeric
# columns and factors, after the recipe) and a 0/1 target y; predict_proba()
# gives the probability of class 1. Columns without information in training
# (constant, or a factor with a single level) are dropped with a message.

MODEL_LABELS <- c(glm = "Régression logistique", ranger = "Forêt aléatoire")

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
  x[] <- lapply(x, function(v) if (is.factor(v)) droplevels(v) else if (is.logical(v)) as.numeric(v) else v)
  dropped <- names(x)[vapply(x, uninformative, logical(1))]
  x <- x[setdiff(names(x), dropped)]
  messages <- if (length(dropped) > 0) {
    paste0("variables sans information dans l'entraînement, retirées : ",
           paste(dropped, collapse = ", "))
  } else {
    character(0)
  }
  if (length(unique(y)) < 2) {
    stop("l'entraînement ne contient qu'une classe de la cible.", call. = FALSE)
  }
  if (ncol(x) == 0) {
    stop("aucune variable explicative informative dans l'entraînement.", call. = FALSE)
  }
  fit <- switch(type, glm = fit_glm(x, y, weights), ranger = fit_ranger(x, y, weights, ...))
  structure(
    list(type = type, fit = fit$model, columns = names(x), levels = fit$levels,
         dropped = dropped, messages = c(messages, fit$messages)),
    class = "fitted_model"
  )
}

# A column is uninformative when it is constant in training (a factor with a
# single level, or a numeric column with a single value).
uninformative <- function(v) {
  if (is.factor(v)) nlevels(v) < 2 else is_constant(v)
}

# Logistic regression. Each factor takes its most frequent training level as
# reference (the alphabetical order depends on the locale). quasibinomial when
# the weights are not integers: same coefficients, without the warning.
fit_glm <- function(x, y, weights) {
  factors <- names(x)[vapply(x, is.factor, logical(1))]
  levels <- lapply(stats::setNames(factors, factors), function(v) {
    counts <- table(x[[v]])
    c(names(counts)[which.max(counts)], setdiff(levels(x[[v]]), names(counts)[which.max(counts)]))
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

# Probability of class 1 for each row of newx.
predict_proba <- function(model, newx) {
  stopifnot(inherits(model, "fitted_model"))
  newx <- as.data.frame(newx)
  missing <- setdiff(model$columns, names(newx))
  if (length(missing) > 0) {
    stop("colonnes absentes : ", paste(missing, collapse = ", "), call. = FALSE)
  }
  newx <- newx[model$columns]
  newx[] <- lapply(newx, function(v) if (is.logical(v)) as.numeric(v) else v)
  with_na <- names(newx)[vapply(newx, anyNA, logical(1))]
  if (length(with_na) > 0) {
    stop("valeurs manquantes après la recette dans : ", paste(with_na, collapse = ", "),
         " ; choisir une imputation dans l'onglet Prétraitement.", call. = FALSE)
  }
  if (model$type == "glm") {
    for (v in names(model$levels)) {
      unseen <- setdiff(unique(as.character(newx[[v]])), c(model$levels[[v]], NA))
      if (length(unseen) > 0) {
        stop("modalité inconnue de l'entraînement dans ", v, " : ", paste(unseen, collapse = ", "),
             call. = FALSE)
      }
      newx[[v]] <- factor(as.character(newx[[v]]), levels = model$levels[[v]])
    }
    return(unname(stats::predict(model$fit, newdata = newx, type = "response")))
  }
  # A fixed seed: predict.ranger() otherwise draws one from the global random
  # state, shared by all sessions of the server.
  unname(stats::predict(model$fit, data = newx, num.threads = 1, seed = 1)$predictions[, "1"])
}

# --- Pipeline of the modelling tab ------------------------------------------

# settings: target, positive (level of the target coded 1), predictors,
# sample_size, group (column name or NULL), mode, test_prop, valid_prop,
# by (column name), test_values, models (types), seed.
# Steps: rows without target removed, target coded 0/1, sample, split, recipe
# fitted on the training set (k - 1 dummies imposed for the logistic
# regression), models, metrics at threshold 0.5 on the validation set.
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

  results <- lapply(stats::setNames(settings$models, settings$models), function(type) {
    model_recipe <- recipe
    notes <- character(0)
    if (type == "glm" && identical(recipe$dummy$method, "one_hot")) {
      model_recipe$dummy$method <- "k_minus_1"
      notes <- "régression logistique : dummification en k − 1 colonnes au lieu de one-hot, colinéaire avec la constante."
    }
    fitted <- fit_recipe(model_recipe, split$train$x, types[predictors])
    y_train <- split$train$y[fitted$kept_rows]
    model <- fit_model(type, fitted$train, y_train, seed = settings$seed)
    x_valid <- apply_recipe(fitted, split$valid$x)
    prob <- predict_proba(model, x_valid)
    cm <- confusion(split$valid$y, prob, 0.5)
    list(model = model, recipe = fitted, prob_valid = prob, confusion = cm,
         metrics = c(precision = precision(cm), recall = recall(cm), f1 = f1(cm)),
         messages = unique(c(notes, fitted$messages, model$messages)))
  })

  # sample_rows: indices of the sample in the data with a target (the split's
  # rows index this sample).
  list(split = split, results = results, messages = messages, sample_rows = rows,
       distribution = target_distribution(split), target = target, positive = settings$positive)
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
