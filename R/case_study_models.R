# Tab 5, modelling part (E13): read from data/final_models.rds, written by
# data-raw/03_final_models.R. Nothing is refitted in the app.

SPEC_LABELS <- c(main = "principale", ablation = "ablation")
BALANCE_LABELS <- c(none = "aucun rééquilibrage", weights = "pondération des classes")

# Stops unless final has the parts the tab reads.
check_final_results <- function(final) {
  stopifnot(
    is.list(final), all(c("meta", "naive_test", "specs", "intervals") %in% names(final)),
    all(c("main", "ablation") %in% names(final$specs)),
    all(vapply(final$specs, function(s) all(c("vars", "test", "roc", "logistic", "importance") %in% names(s)),
               logical(1))),
    all(c("quantity", "estimate", "lower", "upper") %in% names(final$intervals))
  )
  invisible(TRUE)
}

# Rebalancing of the variant chosen on 2023 for each model of a specification.
chosen_balance <- function(spec) {
  chosen <- spec$test[spec$test$chosen, , drop = FALSE]
  stats::setNames(chosen$balance, chosen$model)
}

# Per model, the evaluation on 2024 of one variant (the chosen one by default)
# in the format of tab 4: threshold, metrics, confusion and roc.
model_evaluations <- function(spec, balance = chosen_balance(spec)) {
  models <- names(balance)
  stats::setNames(lapply(models, function(type) {
    row <- spec$test[spec$test$model == type & spec$test$balance == balance[[type]], , drop = FALSE]
    stopifnot(nrow(row) == 1)
    roc <- spec$roc[[type]][[balance[[type]]]]
    list(threshold = row$threshold,
         metrics = c(precision = row$precision, recall = row$recall, f1 = row$f1),
         confusion = c(tp = row$tp, fp = row$fp, fn = row$fn, tn = row$tn),
         roc = list(fpr = roc$fpr, tpr = roc$tpr, auc = row$auc),
         setting = row$setting, balance = row$balance)
  }), models)
}

# Naive references of the test year, in the format of comparison_table().
naive_reference <- function(final) {
  list(constant_auc = final$naive_test$constant_auc,
       all_positive = list(metrics = final$naive_test$all_positive))
}

variant_name <- function(spec, model, balance) paste(spec, model, balance)

# Estimate and 95 % percentile interval of a quantity of the bootstrap table
# ("main glm weights", or a difference "a - b").
interval_of <- function(final, quantity) {
  row <- final$intervals[final$intervals$quantity == quantity, , drop = FALSE]
  stopifnot(nrow(row) == 1)
  c(estimate = row$estimate, lower = row$lower, upper = row$upper)
}

format_interval <- function(x, digits = 3) {
  sprintf("%s [%s ; %s]", format_number(x[["estimate"]], digits),
          format_number(x[["lower"]], digits), format_number(x[["upper"]], digits))
}

# Chosen variants of a specification on 2024: setting and rebalancing chosen
# on 2023, threshold of 2023, AUC with its interval, metrics at the threshold.
test_table <- function(final, spec_name) {
  spec <- final$specs[[spec_name]]
  ev <- model_evaluations(spec)
  rows <- lapply(names(ev), function(type) {
    e <- ev[[type]]
    auc <- interval_of(final, variant_name(spec_name, type, e$balance))
    data.frame(Modèle = MODEL_LABELS[[type]],
               `Choisi sur 2023` = paste0(if (e$setting != "-") paste0(e$setting, ", ") else "",
                                          BALANCE_LABELS[[e$balance]]),
               `Seuil (2023)` = format_number(e$threshold, 3),
               `AUC [IC 95 %]` = format_interval(auc),
               Precision = format_metric(e$metrics[["precision"]]),
               Recall = format_metric(e$metrics[["recall"]]),
               F1 = format_metric(e$metrics[["f1"]]), check.names = FALSE)
  })
  n <- final$naive_test$all_positive
  rbind(do.call(rbind, rows),
        data.frame(Modèle = "Référence : score constant", `Choisi sur 2023` = "", `Seuil (2023)` = "",
                   `AUC [IC 95 %]` = format_metric(final$naive_test$constant_auc), Precision = "",
                   Recall = "", F1 = "", check.names = FALSE),
        data.frame(Modèle = "Référence : tout positif", `Choisi sur 2023` = "", `Seuil (2023)` = "",
                   `AUC [IC 95 %]` = "", Precision = format_metric(n[["precision"]]),
                   Recall = format_metric(n[["recall"]]), F1 = format_metric(n[["f1"]]),
                   check.names = FALSE))
}

# Differences of AUC between the chosen models of a specification.
model_difference_table <- function(final, spec_name) {
  b <- chosen_balance(final$specs[[spec_name]])
  pairs <- list(c("ranger", "glm"), c("ranger", "rpart"), c("glm", "rpart"))
  rows <- lapply(pairs, function(p) {
    q <- paste(variant_name(spec_name, p[1], b[[p[1]]]), "-", variant_name(spec_name, p[2], b[[p[2]]]))
    data.frame(Écart = paste(MODEL_LABELS[[p[1]]], "moins", tolower(MODEL_LABELS[[p[2]]])),
               `Écart d'AUC [IC 95 %]` = format_interval(interval_of(final, q), 4),
               check.names = FALSE)
  })
  do.call(rbind, rows)
}

# Weighting against no rebalancing, each with its own setting and threshold
# chosen on 2023 and evaluated on 2024.
balance_table <- function(final, spec_name) {
  spec <- final$specs[[spec_name]]
  rows <- lapply(names(MODEL_LABELS), function(type) {
    t <- spec$test[spec$test$model == type, , drop = FALSE]
    none <- t[t$balance == "none", ]
    weights <- t[t$balance == "weights", ]
    q <- paste(variant_name(spec_name, type, "weights"), "-", variant_name(spec_name, type, "none"))
    data.frame(Modèle = MODEL_LABELS[[type]], `AUC sans` = format_metric(none$auc),
               `AUC pondérée` = format_metric(weights$auc),
               `Écart [IC 95 %]` = format_interval(interval_of(final, q), 4),
               `Seuil sans` = format_number(none$threshold, 3),
               `Seuil pondéré` = format_number(weights$threshold, 3),
               `F1 sans` = format_metric(none$f1), `F1 pondéré` = format_metric(weights$f1),
               check.names = FALSE)
  })
  do.call(rbind, rows)
}

# Ablation: the chosen variant of each model, with and without the variables
# of the crash.
ablation_table <- function(final) {
  main <- chosen_balance(final$specs$main)
  abl <- chosen_balance(final$specs$ablation)
  rows <- lapply(names(MODEL_LABELS), function(type) {
    a <- variant_name("ablation", type, abl[[type]])
    m <- variant_name("main", type, main[[type]])
    data.frame(Modèle = MODEL_LABELS[[type]],
               `AUC principale` = format_interval(interval_of(final, m)),
               `AUC avec le choc` = format_interval(interval_of(final, a)),
               `Écart [IC 95 %]` = format_interval(interval_of(final, paste(a, "-", m)), 4),
               check.names = FALSE)
  })
  do.call(rbind, rows)
}

# Variables of the ablation that the main specification does not have.
crash_vars <- function(final) setdiff(final$specs$ablation$vars, final$specs$main$vars)

# "a, b et c".
enumerate_fr <- function(x) {
  if (length(x) <= 1) return(paste(x))
  paste(paste(x[-length(x)], collapse = ", "), "et", x[length(x)])
}

# Odds ratio of one term of the logistic regression without rebalancing.
odds_ratio_of <- function(spec, term) {
  row <- spec$logistic[spec$logistic$term == term, , drop = FALSE]
  stopifnot(nrow(row) == 1)
  c(estimate = row$odds_ratio, lower = row$lower, upper = row$upper)
}

# Variables to remove from "Variables explicatives" in tab 4 so that only the
# list of the study is kept: every candidate of the sample (quanti and quali
# but the target) that is not in vars.
replay_removed <- function(ds, vars) {
  types <- ds$var_types
  candidates <- setdiff(names(types)[types %in% c("quanti", "quali")], CASE_STUDY_TARGET)
  setdiff(candidates, vars)
}
