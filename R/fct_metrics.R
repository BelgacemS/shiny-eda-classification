# Metrics at a decision threshold (PLAN.md, step E10a). A row is predicted
# positive when its score is at least the threshold.

# Confusion counts of a 0/1 target and scores: named integer vector
# c(tp, fp, fn, tn) with class "confusion".
confusion <- function(y, score, threshold = 0.5) {
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("le seuil doit être un nombre.", call. = FALSE)
  }
  check_scores(y, score)
  predicted <- score >= threshold
  actual <- y == 1
  structure(
    c(tp = sum(predicted & actual), fp = sum(predicted & !actual),
      fn = sum(!predicted & actual), tn = sum(!predicted & !actual)),
    class = "confusion"
  )
}

# Precision = TP / (TP + FP); NA when no row is predicted positive.
precision <- function(cm) {
  predicted <- cm[["tp"]] + cm[["fp"]]
  if (predicted == 0) NA_real_ else cm[["tp"]] / predicted
}

# Recall = TP / (TP + FN); NA when there is no actual positive.
recall <- function(cm) {
  actual <- cm[["tp"]] + cm[["fn"]]
  if (actual == 0) NA_real_ else cm[["tp"]] / actual
}

# F1 = 2 TP / (2 TP + FP + FN), the harmonic mean of precision and recall
# written so that it stays defined when one of them is not: 0 when nothing is
# predicted positive but positives exist, NA only with no actual and no
# predicted positive.
f1 <- function(cm) {
  denominator <- 2 * cm[["tp"]] + cm[["fp"]] + cm[["fn"]]
  if (denominator == 0) NA_real_ else 2 * cm[["tp"]] / denominator
}

# 2 x 2 table for display: rows = observed, columns = predicted, positive first.
confusion_table <- function(cm) {
  # Filled by column: predicted positive (TP, FP), then predicted negative (FN, TN).
  matrix(c(cm[["tp"]], cm[["fp"]], cm[["fn"]], cm[["tn"]]), nrow = 2,
         dimnames = list(Observé = c("positif", "négatif"), Prédit = c("positif", "négatif")))
}

check_scores <- function(y, score) {
  if (length(y) != length(score)) {
    stop("y et les scores n'ont pas la même longueur.", call. = FALSE)
  }
  if (anyNA(y) || !all(y %in% c(0, 1)) || anyNA(score)) {
    stop("la cible doit valoir 0 ou 1 et les scores être renseignés.", call. = FALSE)
  }
}

# Confusion counts, precision, recall and F1 at one threshold.
threshold_metrics <- function(y, score, threshold) {
  cm <- confusion(y, score, threshold)
  list(threshold = threshold, confusion = cm,
       metrics = c(precision = precision(cm), recall = recall(cm), f1 = f1(cm)))
}

# --- ROC, AUC and threshold choice (PLAN.md, step E11a) ----------------------

# ROC curve and AUC by pROC, with levels and direction fixed: "auto" would
# flip the curve of a model worse than chance and inflate its AUC (Robin et
# al., 2011). Higher scores mean class 1. list(auc, fpr, tpr), fpr increasing;
# NA and no curve when one class is missing.
roc_curve <- function(y, score) {
  check_scores(y, score)
  if (length(unique(y)) < 2) {
    return(list(auc = NA_real_, fpr = numeric(0), tpr = numeric(0)))
  }
  r <- pROC::roc(response = y, predictor = score, levels = c(0, 1), direction = "<", quiet = TRUE)
  list(auc = as.numeric(r$auc), fpr = rev(1 - r$specificities), tpr = rev(r$sensitivities))
}

# Thresholds offered in the app: the threshold sliders move on this grid, so
# the threshold chosen here is exactly a value the slider can show.
THRESHOLD_GRID <- round(seq(0, 1, by = 0.001), 3)

# F1 at every threshold of grid (a row is predicted positive when score >= t).
# Counts of scores >= t come from the sorted scores: findInterval(left.open =
# TRUE) gives the number of values strictly below t.
f1_curve <- function(y, score, grid = THRESHOLD_GRID) {
  check_scores(y, score)
  pos <- sort(score[y == 1])
  neg <- sort(score[y == 0])
  tp <- length(pos) - findInterval(grid, pos, left.open = TRUE)
  fp <- length(neg) - findInterval(grid, neg, left.open = TRUE)
  fn <- length(pos) - tp
  denominator <- 2 * tp + fp + fn
  values <- 2 * tp / denominator
  values[denominator == 0] <- NA_real_
  values
}

# Threshold of maximal F1 on the grid; the smallest one in case of a tie.
best_f1_threshold <- function(y, score, grid = THRESHOLD_GRID) {
  values <- f1_curve(y, score, grid)
  if (all(is.na(values))) {
    return(0.5)
  }
  grid[which.max(values)]
}

# Naive references for a comparison table: a constant score (AUC 0.5 whatever
# the constant) and the rule "every row positive" (precision = prevalence,
# recall = 1, F1 = 2 p / (1 + p)).
naive_metrics <- function(y) {
  list(constant_auc = roc_curve(y, rep(0.5, length(y)))$auc,
       all_positive = threshold_metrics(y, rep(1, length(y)), 0.5))
}

# --- Leak alert ----------------------------------------------------------------

# Above this univariate AUC a predictor is almost a copy of the target.
LEAK_AUC <- 0.95
# "Fuite probable" also needs this many evaluation rows of each class: the
# DeLong variance is 0 when the AUC is exactly 0 or 1, so the interval
# collapses to [1, 1] whatever the size; with 4 positives and 6 negatives a
# variable of true AUC 0.9 reaches an AUC of 1 in about 30 % of draws. At 30
# rows per class the standard error near 0.95 is about 0.03.
LEAK_MIN_PER_CLASS <- 30

# AUC of each predictor taken alone, learned on the training set and measured
# on an evaluation set, then max(AUC, 1 - AUC): the alert measures the
# strength of the link, not its direction. Quanti: the value itself, on the
# complete cases. Quali: the share of positives of the level in training, NA
# as a level of its own, a level unseen in training scored with the training
# prevalence (so a column with one level per row stays near 0.5 instead of
# being learned on its own rows). 0.5 for a constant column, NA for a column
# without values or when the evaluation set has a single class; never an error.
univariate_auc <- function(x_train, y_train, x_eval, y_eval) {
  details <- univariate_auc_details(x_train, y_train, x_eval, y_eval)
  stats::setNames(details$auc, details$variable)
}

# Same AUC per variable, with the lower bound of the 95 % DeLong interval of
# max(AUC, 1 - AUC) (DeLong et al., 1988) and the evaluation rows of each
# class used (complete cases for a quanti variable). The bound of the
# symmetrized AUC is the lower bound of the AUC when it is at least 0.5, else
# 1 minus its upper bound. NA when it cannot be computed (one row of a class).
univariate_auc_details <- function(x_train, y_train, x_eval, y_eval) {
  x_train <- as.data.frame(x_train)
  x_eval <- as.data.frame(x_eval)
  empty <- list(auc = NA_real_, lower = NA_real_, n_pos = 0L, n_neg = 0L)
  rows <- lapply(names(x_train), function(v) {
    tryCatch(univariate_auc_one(x_train[[v]], y_train, x_eval[[v]], y_eval),
             error = function(e) empty)
  })
  data.frame(variable = names(x_train),
             auc = vapply(rows, `[[`, numeric(1), "auc"),
             lower = vapply(rows, `[[`, numeric(1), "lower"),
             n_pos = vapply(rows, function(r) as.integer(r$n_pos), integer(1)),
             n_neg = vapply(rows, function(r) as.integer(r$n_neg), integer(1)))
}

# Variables to report: `leak` when the bound exceeds LEAK_AUC with at least
# LEAK_MIN_PER_CLASS rows of each class; `caution` when the AUC exceeds
# LEAK_AUC without meeting both conditions (set too small to conclude).
leak_status <- function(details) {
  strong <- !is.na(details$auc) & details$auc > LEAK_AUC
  sure <- strong & !is.na(details$lower) & details$lower > LEAK_AUC &
    details$n_pos >= LEAK_MIN_PER_CLASS & details$n_neg >= LEAK_MIN_PER_CLASS
  list(leak = details$variable[sure], caution = details$variable[strong & !sure])
}

univariate_auc_one <- function(train, y_train, eval, y_eval) {
  none <- list(auc = NA_real_, lower = NA_real_, n_pos = 0L, n_neg = 0L)
  if (is.null(eval) || all(is.na(train)) || all(is.na(eval))) {
    return(none)
  }
  if (is.numeric(eval)) {
    keep <- !is.na(eval)
    score <- eval[keep]
    y <- y_eval[keep]
  } else {
    na_level <- "\u0001NA"
    key <- function(v) ifelse(is.na(v), na_level, as.character(v))
    rates <- tapply(y_train, key(train), mean)
    score <- unname(rates[key(eval)])
    score[is.na(score)] <- mean(y_train)
    y <- y_eval
  }
  counts <- list(n_pos = sum(y == 1), n_neg = sum(y == 0))
  if (length(unique(y)) < 2) {
    return(c(list(auc = NA_real_, lower = NA_real_), counts))
  }
  if (is_constant(score)) {
    return(c(list(auc = 0.5, lower = NA_real_), counts))
  }
  r <- pROC::roc(response = y, predictor = score, levels = c(0, 1), direction = "<", quiet = TRUE)
  auc <- as.numeric(r$auc)
  # pROC warns that an interval at AUC 0 or 1 is degenerate: the minimum
  # number of rows per class handles that case.
  ci <- withCallingHandlers(
    tryCatch(as.numeric(pROC::ci.auc(r, method = "delong")), error = function(e) rep(NA_real_, 3)),
    warning = function(w) invokeRestart("muffleWarning"))
  lower <- if (auc >= 0.5) ci[1] else 1 - ci[3]
  c(list(auc = max(auc, 1 - auc), lower = lower), counts)
}
