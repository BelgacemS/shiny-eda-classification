# Metrics at a decision threshold (PLAN.md, step E10a). A row is predicted
# positive when its score is at least the threshold.

# Confusion counts of a 0/1 target and scores: named integer vector
# c(tp, fp, fn, tn) with class "confusion".
confusion <- function(y, score, threshold = 0.5) {
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("le seuil doit être un nombre.", call. = FALSE)
  }
  if (length(y) != length(score)) {
    stop("y et les scores n'ont pas la même longueur.", call. = FALSE)
  }
  if (anyNA(y) || !all(y %in% c(0, 1)) || anyNA(score)) {
    stop("la cible doit valoir 0 ou 1 et les scores être renseignés.", call. = FALSE)
  }
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
