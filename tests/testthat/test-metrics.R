# Expected values computed by hand in the comments.

test_that("metrics equal those of a confusion matrix built by hand", {
  # Threshold 0.5: predicted positive for 0.9, 0.6, 0.7, 0.55, 0.8.
  # y = 1 and predicted 1: rows 1, 2, 8 (TP = 3); y = 1 and predicted 0: row 3 (FN = 1);
  # y = 0 and predicted 1: rows 4, 7 (FP = 2); y = 0 and predicted 0: rows 5, 6 (TN = 2).
  y <- c(1, 1, 1, 0, 0, 0, 0, 1)
  score <- c(0.9, 0.6, 0.4, 0.7, 0.2, 0.1, 0.55, 0.8)
  cm <- confusion(y, score, 0.5)
  expect_equal(unclass(cm), c(tp = 3L, fp = 2L, fn = 1L, tn = 2L))
  expect_equal(precision(cm), 3 / 5)
  expect_equal(recall(cm), 3 / 4)
  # 2 P R / (P + R) = 2 * 0.6 * 0.75 / 1.35 = 2 / 3, equal to 2 TP / (2 TP + FP + FN) = 6 / 9.
  expect_equal(f1(cm), 2 / 3)
  expect_equal(f1(cm), 2 * precision(cm) * recall(cm) / (precision(cm) + recall(cm)))
  tab <- confusion_table(cm)
  expect_equal(tab["positif", "positif"], 3)
  expect_equal(tab["négatif", "positif"], 2)  # observed negative, predicted positive: FP
  expect_equal(tab["positif", "négatif"], 1)  # observed positive, predicted negative: FN
  expect_equal(sum(tab), length(y))
})

test_that("a score equal to the threshold is predicted positive", {
  cm <- confusion(c(1, 0), c(0.5, 0.5), 0.5)
  expect_equal(unclass(cm), c(tp = 1L, fp = 1L, fn = 0L, tn = 0L))
})

test_that("no predicted positive: precision NA, recall and F1 zero", {
  cm <- confusion(c(1, 1, 0), c(0.1, 0.2, 0.3), 0.5)
  expect_true(is.na(precision(cm)))
  expect_equal(recall(cm), 0)
  expect_equal(f1(cm), 0)
})

test_that("no actual positive: recall NA; F1 zero with false positives, NA without", {
  cm <- confusion(c(0, 0, 0), c(0.9, 0.2, 0.3), 0.5)
  expect_true(is.na(recall(cm)))
  expect_equal(precision(cm), 0)
  expect_equal(f1(cm), 0)
  none <- confusion(c(0, 0), c(0.1, 0.2), 0.5)
  expect_true(is.na(precision(none)))
  expect_true(is.na(recall(none)))
  expect_true(is.na(f1(none)))
})

test_that("recall does not increase when the threshold rises", {
  withr::local_seed(8)
  y <- rbinom(200, 1, 0.3)
  score <- runif(200)
  recalls <- vapply(seq(0, 1, by = 0.05), function(t) recall(confusion(y, score, t)), numeric(1))
  expect_true(all(diff(recalls) <= 0))
})

test_that("invalid inputs are refused with a message", {
  expect_error(confusion(c(1, 2), c(0.1, 0.2)), "0 ou 1")
  expect_error(confusion(c(1, NA), c(0.1, 0.2)), "0 ou 1")
  expect_error(confusion(c(1, 0), c(0.1, NA)), "renseignés")
  expect_error(confusion(c(1, 0), 0.1), "longueur")
  expect_error(confusion(c(1, 0), c(0.1, 0.9), threshold = c(0.2, 0.5)), "seuil")
  expect_error(confusion(c(1, 0), c(0.1, 0.9), threshold = NA), "seuil")
})

# --- E11a: ROC, AUC, threshold choice, naive references, leak alert ---------------

test_that("the AUC of pROC equals W / (n1 n0) on a toy set with ties", {
  y <- c(0, 0, 1, 1, 0, 1, 1, 0, 1, 0)
  score <- c(0.1, 0.4, 0.4, 0.8, 0.4, 0.2, 0.9, 0.3, 0.4, 0.1)
  w <- stats::wilcox.test(score[y == 1], score[y == 0], exact = FALSE)$statistic
  # By hand: pairs (positive, negative) with the positive higher count 1, ties 1/2.
  pos <- score[y == 1]
  neg <- score[y == 0]
  by_hand <- sum(outer(pos, neg, ">")) + 0.5 * sum(outer(pos, neg, "=="))
  expect_equal(unname(w), by_hand)
  expect_equal(roc_curve(y, score)$auc, unname(w) / (length(pos) * length(neg)))
})

test_that("the ROC direction is fixed: a reversed score gets 1 - AUC, not a flipped curve", {
  withr::local_seed(17)
  y <- rbinom(300, 1, 0.4)
  score <- y + rnorm(300)
  auc <- roc_curve(y, score)$auc
  expect_gt(auc, 0.6)
  expect_equal(roc_curve(y, -score)$auc, 1 - auc)
  curve <- roc_curve(y, score)
  expect_equal(range(curve$fpr), c(0, 1))
  expect_true(all(diff(curve$fpr) >= 0))
  expect_true(is.na(roc_curve(c(1, 1), c(0.2, 0.4))$auc))
})

test_that("the F1 curve equals F1 at each threshold, and the chosen threshold maximises it", {
  withr::local_seed(18)
  y <- rbinom(500, 1, 0.25)
  score <- round(plogis(2 * y + rnorm(500)), 2)  # many ties
  values <- f1_curve(y, score)
  by_threshold <- vapply(THRESHOLD_GRID, function(t) f1(confusion(y, score, t)), numeric(1))
  expect_equal(values, by_threshold)
  best <- best_f1_threshold(y, score)
  expect_true(best %in% THRESHOLD_GRID)
  expect_equal(f1(confusion(y, score, best)), max(by_threshold, na.rm = TRUE))
  # Ties: the smallest threshold of maximal F1.
  expect_equal(best, THRESHOLD_GRID[which(by_threshold == max(by_threshold, na.rm = TRUE))[1]])
})

test_that("recall does not increase along the threshold grid", {
  withr::local_seed(19)
  y <- rbinom(300, 1, 0.3)
  score <- runif(300)
  recalls <- vapply(THRESHOLD_GRID, function(t) recall(confusion(y, score, t)), numeric(1))
  expect_true(all(diff(recalls) <= 0))
})

test_that("naive references: AUC 0.5 for a constant score, prevalence and recall 1 for all positive", {
  y <- c(1, 0, 0, 1, 0, 0, 0, 0)
  naive <- naive_metrics(y)
  expect_equal(naive$constant_auc, 0.5)
  m <- naive$all_positive$metrics
  expect_equal(m[["precision"]], 2 / 8)
  expect_equal(m[["recall"]], 1)
  expect_equal(m[["f1"]], 2 * 0.25 / 1.25)
})

test_that("univariate AUC flags a copy of the target and spares independent or unique-level columns", {
  withr::local_seed(20)
  n_train <- 400
  n_eval <- 200
  y_train <- rbinom(n_train, 1, 0.3)
  y_eval <- rbinom(n_eval, 1, 0.3)
  make <- function(y, offset) data.frame(
    copy = factor(ifelse(y == 1, "oui", "non")),
    increasing = 10 * y + runif(length(y)),
    noise = rnorm(length(y)),
    noise_quali = factor(sample(c("a", "b", "c"), length(y), TRUE)),
    unique_level = factor(paste0("id", offset + seq_along(y))),
    constant = 1,
    all_na = NA_real_,
    text = sample(c("u", "v"), length(y), TRUE),
    stringsAsFactors = FALSE)
  x_train <- make(y_train, 0)
  x_eval <- make(y_eval, n_train)
  auc <- univariate_auc(x_train, y_train, x_eval, y_eval)
  expect_equal(auc[["copy"]], 1)
  expect_equal(auc[["increasing"]], 1)
  expect_lt(auc[["noise"]], LEAK_AUC)
  expect_lt(auc[["noise_quali"]], LEAK_AUC)
  # Every evaluation level is unseen in training: scored with the prevalence, constant.
  expect_equal(auc[["unique_level"]], 0.5)
  expect_equal(auc[["constant"]], 0.5)
  expect_true(is.na(auc[["all_na"]]))
  expect_false(is.na(auc[["text"]]))
  # Same value for x and -x: the alert measures strength, not direction.
  negated <- x_eval
  negated$noise <- -negated$noise
  expect_equal(univariate_auc(x_train, y_train, negated, y_eval)[["noise"]], auc[["noise"]])
  # A single class in the evaluation set gives NA, without error.
  expect_true(all(is.na(univariate_auc(x_train, y_train, x_eval, rep(0, n_eval)))))
})

test_that("NA is a level of its own and unseen levels take the training prevalence", {
  # Training: level a always positive, NA always negative; b unseen in training.
  x_train <- data.frame(g = factor(c("a", "a", NA, NA)))
  y_train <- c(1, 1, 0, 0)
  x_eval <- data.frame(g = factor(c("a", NA, "b", "b"), levels = c("a", "b")))
  y_eval <- c(1, 0, 1, 0)
  # Scores: a = 1, NA = 0, b = prevalence 0.5. Pairs (pos, neg): (a, NA) 1,
  # (a, b) 1, (b, NA) 1, (b, b) 1/2: AUC = 3.5 / 4.
  expect_equal(univariate_auc(x_train, y_train, x_eval, y_eval)[["g"]], 3.5 / 4)
})

test_that("a quanti predictor with missing values is scored on its complete cases", {
  x_train <- data.frame(v = c(1, 2, NA, 4))
  y_train <- c(0, 1, 0, 1)
  x_eval <- data.frame(v = c(1, NA, 3, 4, NA))
  y_eval <- c(0, 0, 1, 1, 1)
  # Complete cases of the evaluation set: (1, 0), (3, 1), (4, 1), so AUC = 1.
  expect_equal(univariate_auc(x_train, y_train, x_eval, y_eval)[["v"]], 1)
})

# --- E11d: DeLong bound and minimum size for the leak alert ---------------------------

test_that("the lower bound is the DeLong one, symmetrized when the AUC is below 0.5", {
  withr::local_seed(50)
  y <- rbinom(200, 1, 0.4)
  score <- y + rnorm(200)
  r <- pROC::roc(response = y, predictor = score, levels = c(0, 1), direction = "<", quiet = TRUE)
  ci <- as.numeric(pROC::ci.auc(r, method = "delong"))
  x_train <- data.frame(up = score, down = -score)
  d <- univariate_auc_details(x_train, y, x_train, y)
  expect_equal(d$lower[d$variable == "up"], ci[1])
  # -score has AUC 1 - AUC: its symmetrized bound is 1 minus the upper bound of its own interval.
  r_down <- pROC::roc(response = y, predictor = -score, levels = c(0, 1), direction = "<", quiet = TRUE)
  ci_down <- as.numeric(pROC::ci.auc(r_down, method = "delong"))
  expect_equal(d$lower[d$variable == "down"], 1 - ci_down[3])
  expect_equal(d$lower[d$variable == "down"], d$lower[d$variable == "up"])
  expect_equal(c(d$n_pos[1], d$n_neg[1]), c(sum(y == 1), sum(y == 0)))
  # The vector output used by E12a is unchanged.
  expect_equal(univariate_auc(x_train, y, x_train, y), stats::setNames(d$auc, d$variable))
})

test_that("a copy of the target on 30 rows per class is a leak", {
  y <- rep(c(0, 1), each = 30)
  x <- data.frame(copy = factor(ifelse(y == 1, "oui", "non")), score = y)
  expect_silent(d <- univariate_auc_details(x, y, x, y))
  expect_equal(d$lower, c(1, 1))
  expect_equal(leak_status(d)$leak, c("copy", "score"))
})

test_that("on 5 positives and 5 negatives, AUC 1 or 0.96 gives caution, not leak", {
  y <- rep(c(0, 1), each = 5)
  # perfect: AUC 1; one_swap: the highest negative above the lowest positive, AUC 24 / 25 = 0.96.
  x <- data.frame(perfect = c(1:5, 6:10), one_swap = c(1, 2, 3, 4, 6, 5, 7, 8, 9, 10))
  expect_silent(d <- univariate_auc_details(x, y, x, y))
  expect_equal(d$auc, c(1, 0.96))
  status <- leak_status(d)
  expect_length(status$leak, 0)
  expect_equal(status$caution, c("perfect", "one_swap"))
})

test_that("with 30 rows per class, an AUC above 0.95 whose bound is not gives caution", {
  # Binormal quantiles 2.6 sd apart: AUC 0.970, DeLong lower bound about 0.935.
  y <- rep(c(0, 1), each = 30)
  x <- data.frame(v = c(stats::qnorm(stats::ppoints(30)), stats::qnorm(stats::ppoints(30)) + 2.6))
  d <- univariate_auc_details(x, y, x, y)
  expect_gt(d$auc, LEAK_AUC)
  expect_lt(d$lower, LEAK_AUC)
  expect_equal(leak_status(d)$caution, "v")
  expect_length(leak_status(d)$leak, 0)
})

test_that("AUC 0 or 1 and a single row of a class give no warning nor error", {
  y <- c(0, 0, 0, 1)
  x <- data.frame(up = c(1, 2, 3, 4), down = c(4, 3, 2, 1))
  expect_silent(d <- univariate_auc_details(x, y, x, y))
  expect_equal(d$auc, c(1, 1))
  expect_equal(d$n_pos, c(1L, 1L))
  status <- leak_status(d)
  expect_length(status$leak, 0)
  expect_equal(status$caution, c("up", "down"))
})
