# Variable importance (PLAN.md, step E11b).
#
# Permutation importance for every model: the drop of AUC when one original
# predictor (before the recipe) is permuted, the prediction going through
# apply_recipe() (Fisher et al., 2019). For the logistic regression, also the
# absolute standardized coefficients and the odds ratios with Wald intervals.

# Drop of AUC on (test_raw, y) when each variable of vars is permuted, n_rep
# times. predict_fn(x_raw) returns the probability of class 1 for raw rows.
# Permutations are drawn with seed without touching the global random state.
# Returns a data.frame(variable, importance, sd) sorted by decreasing importance;
# importance = AUC of the intact data minus the mean AUC after permutation.
perm_importance <- function(predict_fn, test_raw, y, vars, n_rep = 3, seed = 1) {
  test_raw <- as.data.frame(test_raw)
  if (length(unique(y)) < 2) {
    stop("l'échantillon du test ne contient qu'une classe de la cible.", call. = FALSE)
  }
  base <- roc_curve(y, predict_fn(test_raw))$auc
  n <- nrow(test_raw)
  # (n, n_rep * length(vars)): one permutation per variable and repetition.
  orders <- with_seed(seed, replicate(n_rep * length(vars), sample.int(n)))
  orders <- matrix(orders, nrow = n)
  drops <- vapply(seq_along(vars), function(j) {
    vapply(seq_len(n_rep), function(r) {
      permuted <- test_raw
      permuted[[vars[j]]] <- test_raw[[vars[j]]][orders[, (j - 1) * n_rep + r]]
      base - roc_curve(y, predict_fn(permuted))$auc
    }, numeric(1))
  }, numeric(n_rep))
  drops <- matrix(drops, nrow = n_rep)
  out <- data.frame(variable = vars, importance = colMeans(drops),
                    sd = if (n_rep > 1) apply(drops, 2, stats::sd) else NA_real_)
  out[order(-out$importance), , drop = FALSE]
}

# Rows of the test set used for the importances: all of them up to size,
# otherwise a sample drawn with seed.
importance_rows <- function(n, size, seed = 1) {
  if (n <= size) {
    return(seq_len(n))
  }
  sort(with_seed(seed, sample.int(n, size)))
}

# Logistic regression: one row per estimable coefficient (intercept excluded),
# with |beta_j| * s_j, s_j the standard deviation (1/n) of column j of the
# design matrix built on the training set after the recipe (x_train), and the
# odds ratios exp(beta) with 95 % Wald intervals from `unweighted`, a fit of
# the same predictors without weights or resampling. Wald intervals assume
# independent observations. se: Wald standard error of the unweighted fit.
# Sorted by decreasing standardized coefficient.
logistic_importance <- function(model, x_train, unweighted = model) {
  stopifnot(model$type == "glm", unweighted$type == "glm")
  design <- design_matrix(model, x_train)
  beta <- stats::coef(model$fit)
  terms <- setdiff(names(beta)[!is.na(beta)], "(Intercept)")
  s <- vapply(terms, function(j) sd_1n(design[, j]), numeric(1))
  odds <- exp(cbind(stats::coef(unweighted$fit), stats::confint.default(unweighted$fit)))
  se <- sqrt(diag(stats::vcov(unweighted$fit)))
  # A term absent from the unweighted fit (level lost by resampling) gets NA.
  idx <- match(terms, rownames(odds))
  out <- data.frame(term = terms, coefficient = unname(beta[terms]), sd = unname(s),
                    standardized = abs(unname(beta[terms])) * unname(s),
                    odds_ratio = odds[idx, 1], lower = odds[idx, 2], upper = odds[idx, 3],
                    se = unname(se[match(terms, names(se))]), row.names = NULL)
  out[order(-out$standardized), , drop = FALSE]
}

# Above this Wald standard error (log-odds scale) a coefficient is not
# interpretable: a rare level or a quasi-complete separation (well estimated
# coefficients have standard errors far below 1).
SEPARATION_SE <- 5

# Design matrix of a fitted logistic regression for new rows of predictors
# (same reference levels and columns as the fit).
design_matrix <- function(model, x) {
  x <- as.data.frame(x)[model$columns]
  x[] <- lapply(x, function(v) if (is.logical(v)) as.numeric(v) else v)
  for (v in names(model$levels)) x[[v]] <- factor(as.character(x[[v]]), levels = model$levels[[v]])
  stats::model.matrix(stats::delete.response(stats::terms(model$fit)), data = x,
                      contrasts.arg = model$fit$contrasts)
}

# Importances of every trained model, computed after the test evaluation:
# permutation importance on `size` rows of the test set (n_rep permutations
# per variable), and for the logistic regression the standardized
# coefficients and odds ratios. The odds ratios come from a fit without
# weights or resampling: refitted on the training set after the recipe when a
# rebalancing was used. timings: seconds per part.
compute_importances <- function(trained, size = 1000, n_rep = 1, seed = 1) {
  test <- trained$split$test
  rows <- importance_rows(length(test$y), size, seed)
  x <- test$x[rows, , drop = FALSE]
  y <- test$y[rows]
  timings <- numeric(0)
  permutation <- lapply(stats::setNames(names(trained$results), names(trained$results)), function(type) {
    r <- trained$results[[type]]
    started <- proc.time()[["elapsed"]]
    predict_fn <- function(raw) predict_proba(r$model, apply_recipe(r$recipe, raw))
    out <- perm_importance(predict_fn, x, y, names(x), n_rep, seed)
    timings[[type]] <<- proc.time()[["elapsed"]] - started
    out
  })
  logistic <- NULL
  if ("glm" %in% names(trained$results)) {
    started <- proc.time()[["elapsed"]]
    r <- trained$results$glm
    unweighted <- r$model
    if (!identical(r$balance$method, "none")) {
      y_train <- trained$split$train$y[r$recipe$kept_rows]
      unweighted <- fit_model("glm", r$recipe$train, y_train)
    }
    logistic <- logistic_importance(r$model, r$recipe$train, unweighted)
    timings[["glm_coefficients"]] <- proc.time()[["elapsed"]] - started
  }
  list(permutation = permutation, logistic = logistic, n_rows = length(rows), n_rep = n_rep,
       timings = timings)
}
