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
})
