# Final BAAC models (PLAN.md, steps E12a and E12b).
# Usage, from the project root, after data-raw/02_prepare_baac.R:
#   Rscript data-raw/03_final_models.R
# Environment variables: TRAIN_FRACTION (share of the 2019-2022 training
# accidents used, 1 by default), RANGER_THREADS (4 by default; a forest is
# the same whatever the number of threads, its seed being fixed).
# Output: data/final_models.rds, results only (no model), read by tab 5;
# with TRAIN_FRACTION below 1, data-raw/out/final_models_<percent>.rds instead.
#
# Split by year: training 2019-2022, validation 2023, test 2024. Settings,
# rebalancing and threshold are chosen on 2023 by select_models(), which only
# receives the training and validation sets; a check permutes the 2024 labels
# and expects the same choices. 2024 is then used once per specification, to
# evaluate the variants already chosen: metrics at the 2023 threshold, ROC,
# logistic coefficients, permutation importance, and AUC intervals by a
# bootstrap of the 2024 accidents.

library(data.table)
for (f in c("fct_stats_base.R", "fct_recipe.R", "fct_split.R", "fct_balance.R",
            "fct_metrics.R", "fct_models.R", "fct_importance.R")) {
  source(file.path("R", f), encoding = "UTF-8")
}
source(file.path("data-raw", "model_vars.R"), encoding = "UTF-8")
options(width = 200)

full_path <- file.path("data-raw", "out", "accidents_full.rds")
stability_path <- file.path("data-raw", "out", "stability_2019_2023.csv")

train_fraction <- as.numeric(Sys.getenv("TRAIN_FRACTION", "1"))
stopifnot(!is.na(train_fraction), train_fraction > 0, train_fraction <= 1)
blind_fraction <- 0.02  # training sample of the blind check
threads <- as.integer(Sys.getenv("RANGER_THREADS", "4"))
seed <- 20261115
train_years <- 2019:2022
valid_year <- 2023
test_year <- 2024
review_auc <- 0.8  # univariate AUC above which a variable is reviewed before being kept
boot_draws <- 1000  # draws of the 2024 accidents for the AUC intervals
importance_size <- 20000  # 2024 users (whole accidents) of the permutation importance
importance_reps <- 3  # permutations per variable
results_path <- if (train_fraction == 1) file.path("data", "final_models.rds") else
  file.path("data-raw", "out", sprintf("final_models_%g.rds", 100 * train_fraction))

# Recipe of the final models, the defaults of the app (PLAN.md, E12c): NA
# imputed by the training median (quanti) or mode (quali), the frequent NA of
# trajet and circ being already a level (model_vars.R); levels under 0.5 % of
# the non-missing training values go to "Autre", on the variables with more
# than two levels (grouping a binary variable would only rename its rare
# level). No scaling or dummy coding:
# glm, ranger and rpart take the factors as they are.
rare_threshold <- 0.005
final_recipe <- function(x) {
  multi <- names(x)[vapply(x, function(v) is.factor(v) && nlevels(v) > 2, logical(1))]
  utils::modifyList(default_recipe(), list(
    impute = list(quanti = "median", quali = "mode"),
    rare = list(threshold = rare_threshold, vars = multi)
  ))
}

# Settings tried on 2023 (Probst et al., 2019: mtry and the node size matter
# most for a forest); the logistic regression has none. Each setting is tried
# without rebalancing and with class weights. Node sizes 50, 200 and 500
# (decided on 17 November, before the full run): on 10 % of the training
# set, 10 was never the best size on 2023 (E12a), and on the whole set one of
# its forests would weigh 2.2 to 2.7 GB, two being kept while a third is
# grown; with ten times more rows, 500 keeps the choice open on the side of
# larger nodes.
settings_grid <- list(
  glm = list(list()),
  ranger = apply(expand.grid(mtry = c(3, 5, 8), min.node.size = c(50, 200, 500)), 1, as.list),
  rpart = lapply(c(0.0003, 0.001, 0.003, 0.01), function(cp) list(cp = cp))
)
balance_choices <- c("none", "weights")

# Smaller grid of the blind check: the code path is the same, and two
# settings per model keep the choice open.
blind_grid <- list(
  glm = list(list()),
  ranger = list(list(mtry = 5, min.node.size = 10), list(mtry = 5, min.node.size = 200)),
  rpart = list(list(cp = 0.001), list(cp = 0.01))
)

elapsed <- function() proc.time()[["elapsed"]]

show <- function(x) {
  print(x, row.names = FALSE, col.names = "top", class = FALSE, print.keys = FALSE, nrows = Inf)
}

setting_label <- function(setting) {
  if (length(setting) == 0) return("-")
  paste(names(setting), unlist(setting), sep = " = ", collapse = ", ")
}

var_types_of <- function(x) {
  vapply(x, function(v) if (is.numeric(v)) "quanti" else "quali", character(1))
}

# --- Data ----------------------------------------------------------------------

# Training and validation parts by year: list(x, y, rows) each, x the
# whitelisted predictors, y the 0/1 target, rows the rows of full. The
# training set is a sample of whole accidents, `fraction` of its rows. The
# test part (2024) is built apart, by test_part(), after the selection.
year_parts <- function(full, fraction, seed) {
  part <- function(rows) {
    list(x = as.data.frame(full[rows, ..model_whitelist]), y = as.integer(full$grave[rows] == "oui"),
         rows = rows)
  }
  train_rows <- which(full$an %in% train_years)
  keep <- sample_rows(length(train_rows), round(fraction * length(train_rows)),
                      groups = full$Num_Acc[train_rows], seed = seed)
  list(train = part(train_rows[keep]), valid = part(which(full$an == valid_year)))
}

# Checks the parts against the years, without the code of year_parts():
# training rows from the training years only, made of whole accidents, the
# validation set being exactly the validation year.
check_parts <- function(full, parts) {
  tr <- parts$train$rows
  stopifnot(
    all(full$an[tr] %in% train_years),
    setequal(tr, which(full$an %in% train_years & full$Num_Acc %in% full$Num_Acc[tr])),
    setequal(parts$valid$rows, which(full$an == valid_year))
  )
  invisible(TRUE)
}

# Share (%) of each level by year, NA as a level; a numeric column only gets
# its NA share. One row per variable and level, one column per year.
level_shares <- function(dt, vars) {
  rbindlist(lapply(vars, function(v) {
    x <- dt[[v]]
    level <- if (is.numeric(x)) ifelse(is.na(x), "<NA>", "(valeur)") else
      ifelse(is.na(x), "<NA>", as.character(x))
    counts <- data.table(an = dt$an, level = level)[, .N, by = .(an, level)]
    counts[, share := round(100 * N / sum(N), 1), by = an]
    cbind(variable = v, dcast(counts, level ~ an, value.var = "share", fill = 0))
  }), fill = TRUE)
}

# --- Model selection on 2023 ---------------------------------------------------

# For each model type and rebalancing, the settings of highest AUC on the
# validation set (first in grid order on a tie) and the threshold of maximal
# F1 on that set: a variant. The chosen variant of a type is the one of
# highest AUC ("none" before "weights" on a tie); the other one is kept for the
# comparison of the two rebalancings. The recipe is learned on the training
# set only. Receives the training and validation parts, never the test set.
# Returns the grid (AUC of every fit), the variants and the choice per type,
# the models of the variants (models[[type]][[balance]]), the fitted recipe,
# and the time and size of every fit.
select_models <- function(train, valid, vars, grid, seed, threads) {
  x_train <- train$x[vars]
  fitted <- fit_recipe(final_recipe(x_train), x_train, var_types_of(x_train))
  y_train <- train$y[fitted$kept_rows]
  x_valid <- apply_recipe(fitted, valid$x[vars])
  rows <- list()
  costs <- list()
  variants <- list()
  chosen <- list()
  models <- list()
  for (type in names(grid)) {
    for (balance in balance_choices) {
      best <- NULL
      balanced <- balance_train(list(train = list(x = fitted$train, y = y_train)), balance,
                                seed = seed)$train
      for (setting in grid[[type]]) {
        extra <- if (type == "ranger") list(num.threads = threads) else list()
        started <- elapsed()
        model <- do.call(fit_model, c(list(type, balanced$x, balanced$y, weights = balanced$weights,
                                           seed = seed), setting, extra))
        fitted_at <- elapsed()
        prob <- predict_proba(model, x_valid)
        predicted_at <- elapsed()
        auc <- roc_curve(valid$y, prob)$auc
        rows[[length(rows) + 1]] <- data.table(
          model = type, balance = balance, setting = setting_label(setting), auc = auc,
          aliased = if (type == "glm") sum(is.na(stats::coef(model$fit))) else NA_integer_,
          non_estimable = length(attr(prob, "non_estimable")))
        costs[[length(costs) + 1]] <- data.table(
          model = type, balance = balance, setting = setting_label(setting),
          fit_seconds = fitted_at - started, predict_seconds = predicted_at - fitted_at,
          forest_mb = if (type == "ranger") as.numeric(object.size(model$fit)) / 1024^2 else NA_real_)
        if (is.null(best) || auc > best$auc) {
          best <- list(auc = auc, setting = setting, model = model, prob = prob)
        }
        rm(model)
      }
      threshold <- best_f1_threshold(valid$y, best$prob)
      at <- threshold_metrics(valid$y, best$prob, threshold)
      variants[[type]][[balance]] <- list(
        balance = balance, setting = best$setting, auc = best$auc, threshold = threshold,
        metrics = at$metrics, confusion = unclass(at$confusion),
        non_estimable = length(attr(best$prob, "non_estimable")))
      models[[type]][[balance]] <- best$model
      rm(best)
    }
    aucs <- vapply(variants[[type]], function(v) v$auc, numeric(1))
    chosen[[type]] <- variants[[type]][[which.max(aucs)]]
  }
  list(grid = rbindlist(rows), variants = variants, chosen = chosen, models = models,
       recipe = fitted, costs = rbindlist(costs))
}

# Recomputes the choices of select_models() from its grid and models, without
# its code: recipe equal to a fit on the training set alone; for each type and
# rebalancing, settings of maximal AUC (first on a tie), AUC and threshold of
# maximal F1 recomputed on the validation set; for each type, the variant of
# maximal AUC over the whole grid (first on a tie).
check_selection <- function(sel, train, valid, vars) {
  x_train <- train$x[vars]
  refit <- fit_recipe(final_recipe(x_train), x_train, var_types_of(x_train))
  stopifnot(identical(refit$params, sel$recipe$params))
  x_valid <- apply_recipe(refit, valid$x[vars])
  for (type in names(sel$chosen)) {
    stopifnot(identical(names(sel$variants[[type]]), balance_choices))
    for (b in balance_choices) {
      v <- sel$variants[[type]][[b]]
      grid <- sel$grid[model == type & balance == b]
      prob <- predict_proba(sel$models[[type]][[b]], x_valid)
      stopifnot(
        v$balance == b, v$auc == max(grid$auc),
        setting_label(v$setting) == grid[which.max(auc)]$setting,
        roc_curve(valid$y, prob)$auc == v$auc,
        v$threshold == best_f1_threshold(valid$y, prob)
      )
    }
    grid <- sel$grid[model == type]
    stopifnot(identical(sel$chosen[[type]], sel$variants[[type]][[grid[which.max(auc)]$balance]]))
  }
  invisible(TRUE)
}

# One row per variant, on the validation set; chosen marks the variant of each type.
variants_table <- function(sel) {
  rbindlist(lapply(names(sel$variants), function(type) {
    rbindlist(lapply(sel$variants[[type]], function(v) {
      data.table(model = type, balance = v$balance, chosen = v$balance == sel$chosen[[type]]$balance,
                 setting = setting_label(v$setting), auc = v$auc, threshold = v$threshold,
                 precision = v$metrics[["precision"]], recall = v$metrics[["recall"]],
                 f1 = v$metrics[["f1"]], non_estimable = v$non_estimable)
    }))
  }))
}

# --- Evaluation on 2024 (step E12b) ------------------------------------------------

# The test year: x the whitelisted predictors, y the 0/1 target, rows the rows
# of full, accident the accident of each row (the unit of the bootstrap and of
# the importance sample). Built after the selection, which never receives it.
test_part <- function(full) {
  rows <- which(full$an == test_year)
  stopifnot(length(rows) > 0, all(full$an[rows] == test_year))
  list(x = as.data.frame(full[rows, ..model_whitelist]), y = as.integer(full$grave[rows] == "oui"),
       rows = rows, accident = full$Num_Acc[rows])
}

# At most n_points points of a ROC curve, taken evenly along the curve; the
# first (0, 0) and the last (1, 1) are kept. The AUC is computed on the full curve.
thin_roc <- function(roc, n_points = 1000) {
  keep <- unique(round(seq(1, length(roc$fpr), length.out = min(n_points, length(roc$fpr)))))
  data.frame(fpr = roc$fpr[keep], tpr = roc$tpr[keep])
}

# Each variant of sel on the test year, at its threshold chosen on 2023: AUC,
# precision, recall, F1, confusion, ROC points and the probabilities (kept for
# the bootstrap, not saved).
evaluate_variants <- function(sel, test, vars) {
  x_test <- apply_recipe(sel$recipe, test$x[vars])
  out <- list()
  for (type in names(sel$variants)) {
    for (b in names(sel$variants[[type]])) {
      v <- sel$variants[[type]][[b]]
      prob <- predict_proba(sel$models[[type]][[b]], x_test)
      roc <- roc_curve(test$y, prob)
      at <- threshold_metrics(test$y, prob, v$threshold)
      out[[type]][[b]] <- list(auc = roc$auc, metrics = at$metrics, confusion = unclass(at$confusion),
                               roc = thin_roc(roc), non_estimable = length(attr(prob, "non_estimable")),
                               prob = as.vector(prob))
    }
  }
  out
}

# One row per variant, on the test year (threshold of 2023).
test_table <- function(sel, evaluated) {
  rbindlist(lapply(names(evaluated), function(type) {
    rbindlist(lapply(names(evaluated[[type]]), function(b) {
      e <- evaluated[[type]][[b]]
      data.table(model = type, balance = b, chosen = b == sel$chosen[[type]]$balance,
                 setting = setting_label(sel$variants[[type]][[b]]$setting),
                 threshold = sel$variants[[type]][[b]]$threshold, auc = e$auc,
                 precision = e$metrics[["precision"]], recall = e$metrics[["recall"]],
                 f1 = e$metrics[["f1"]], tp = e$confusion[["tp"]], fp = e$confusion[["fp"]],
                 fn = e$confusion[["fn"]], tn = e$confusion[["tn"]], non_estimable = e$non_estimable)
    }))
  }))
}

# Standardized coefficients of the chosen logistic regression, and odds ratios
# with 95 % Wald intervals from the variant without rebalancing (same
# predictors, recipe and reference levels; logistic_importance() of
# R/fct_importance.R). Wald intervals assume independent users, which users of
# one accident are not: a limit of the study.
logistic_table <- function(sel) {
  chosen <- sel$models$glm[[sel$chosen$glm$balance]]
  unweighted <- sel$models$glm$none
  stopifnot(identical(chosen$levels, unweighted$levels))
  logistic_importance(chosen, sel$recipe$train, unweighted)
}

# Permutation importance (R/fct_importance.R) of the chosen variant of each
# type on the rows `rows` of the test year: drop of AUC when one original
# predictor is permuted before the recipe.
test_importances <- function(sel, test, vars, rows, n_rep, seed) {
  x <- test$x[rows, vars]
  y <- test$y[rows]
  lapply(stats::setNames(names(sel$chosen), names(sel$chosen)), function(type) {
    model <- sel$models[[type]][[sel$chosen[[type]]$balance]]
    started <- elapsed()
    out <- perm_importance(function(raw) predict_proba(model, apply_recipe(sel$recipe, raw)),
                           x, y, vars, n_rep, seed)
    cat(sprintf("Permutation importance, %s: %.1f s\n", type, elapsed() - started))
    out
  })
}

# Sort of a score for weighted_auc(): order of the rows by increasing score,
# and the last sorted position of each group of tied scores.
score_sort <- function(score) {
  o <- order(score)
  sorted <- score[o]
  list(order = o, ends = c(which(diff(sorted) != 0), length(sorted)))
}

# AUC of a score with integer weights w (the number of times each row is
# drawn), ties counted 1/2: sum over the positives of the weight of the
# negatives with a lower score, plus half the tied ones, divided by
# W_pos * W_neg. sorted: score_sort() of the score. Equals the AUC of the
# rows repeated w times.
weighted_auc <- function(y, sorted, w) {
  ws <- w[sorted$order]
  wp <- ws * y[sorted$order]
  pos <- diff(c(0, cumsum(wp)[sorted$ends]))  # weight of the positives of each tie group
  neg <- diff(c(0, cumsum(ws - wp)[sorted$ends]))
  sum(pos * (cumsum(neg) - neg / 2)) / (sum(pos) * sum(neg))
}

# Bootstrap by accident on the test year: B draws of the accidents with
# replacement, every user of a drawn accident coming with it (the users of
# one accident are not independent). Returns a (B, length(scores)) matrix of
# AUC, one column per score, the same draws for every score so that the
# differences between scores are paired.
cluster_bootstrap_auc <- function(y, scores, accident, B, seed) {
  g <- match(accident, unique(accident))
  n_g <- max(g)
  sorts <- lapply(scores, score_sort)
  with_seed(seed, {
    out <- matrix(NA_real_, B, length(scores), dimnames = list(NULL, names(scores)))
    for (b in seq_len(B)) {
      w <- tabulate(sample.int(n_g, n_g, replace = TRUE), n_g)[g]
      out[b, ] <- vapply(sorts, function(s) weighted_auc(y, s, w), numeric(1))
    }
    out
  })
}

# Point estimate on the test year and 95 % percentile interval of each AUC
# and of each difference (a - b) of AUC.
interval_table <- function(estimate, draws, differences) {
  one <- function(quantity, est, d) {
    q <- stats::quantile(d, c(0.025, 0.975), names = FALSE)
    data.table(quantity = quantity, estimate = est, lower = q[1], upper = q[2])
  }
  rbind(
    rbindlist(lapply(names(estimate), function(k) one(k, estimate[[k]], draws[, k]))),
    rbindlist(lapply(differences, function(p) {
      one(paste(p[1], "-", p[2]), estimate[[p[1]]] - estimate[[p[2]]], draws[, p[1]] - draws[, p[2]])
    }))
  )
}

# --- Stability of the coding -----------------------------------------------------

started_all <- elapsed()
raw <- as.data.table(readRDS(full_path))
cat(sprintf("%s: %d users, %d columns\n", full_path, nrow(raw), ncol(raw)))

# 2019 to 2023 only, every candidate column, before the recodings of
# model_vars.R. Printed: NA share of each variable, then the levels whose
# share moves by at least 3 points, or by a factor above 2 with a share of at
# least 1 % one year. Full table written next to the data. dep is left out
# (96 levels, excluded for another reason).
candidates <- setdiff(names(raw), c(model_forbidden, "dep"))
stability <- level_shares(raw[an <= valid_year], candidates)
fwrite(stability, stability_path)
years <- as.character(min(train_years):valid_year)
shares <- as.matrix(stability[, ..years])
moves <- apply(shares, 1, function(s) max(s) - min(s) >= 3 || (max(s) >= 1 && max(s) > 2 * max(min(s), 0.05)))
cat("\n## NA share by year, 2019-2023 (%)\n\n")
show(stability[level == "<NA>" & apply(shares, 1, max) > 0, -"level"])
cat("\n## Level shares that move between years, 2019-2023 (%)\n\n")
show(stability[moves & level != "<NA>"])
cat(sprintf("\nFull table: %s (%d rows)\n", stability_path, nrow(stability)))

full <- recode_model_vars(raw)
rm(raw)
cat("\n## Recoded vosp and catr, 2019-2023 (%)\n\n")
show(level_shares(full[an <= valid_year], c("vosp", "catr")))

check_model_vars(model_vars_main)
check_model_vars(model_vars_ablation)
check_columns_classified(names(full))
cat("\nWhitelist checked: every column whitelisted, excluded or forbidden once; no identifier,",
    "year or target, no catv next to catv_groupe.\n")

# --- Leak control on 2023 --------------------------------------------------------

# On the whole 2019-2022 training set and the original variables: univariate
# AUC (learned on training, measured on 2023) and NA share by class on 2023.
parts_all <- year_parts(full, 1, seed)
check_parts(full, parts_all)
t0 <- elapsed()
leak <- as.data.table(univariate_auc_details(parts_all$train$x, parts_all$train$y,
                                             parts_all$valid$x, parts_all$valid$y))
is_grave <- parts_all$valid$y == 1
na_grave <- vapply(parts_all$valid$x, function(v) 100 * mean(is.na(v[is_grave])), numeric(1))
na_other <- vapply(parts_all$valid$x, function(v) 100 * mean(is.na(v[!is_grave])), numeric(1))
leak[, `:=`(na_grave_pct = round(na_grave[variable], 2), na_other_pct = round(na_other[variable], 2),
            review = ifelse(auc > review_auc, "TO REVIEW", ""))]
cat(sprintf("\n## Leak control on %d: univariate AUC and NA share by class (%%), %.1f s\n\n",
            valid_year, elapsed() - t0))
show(leak[order(-auc), .(variable, auc = round(auc, 4), lower = round(lower, 4), n_pos, n_neg,
                         na_grave_pct, na_other_pct, review)])
stopifnot(setequal(leak$variable, model_whitelist), !anyNA(leak$auc),
          !any(leak$auc > review_auc))
rm(parts_all)
invisible(gc(full = TRUE))

# --- Blind check -------------------------------------------------------------------

# The selection with the 2024 labels permuted gives the same grid and choices.
blind_run <- function(data) {
  p <- year_parts(data, blind_fraction, seed)
  check_parts(data, p)
  lapply(list(main = model_vars_main, ablation = model_vars_ablation), function(vars) {
    s <- select_models(p$train, p$valid, vars, blind_grid, seed, threads)
    check_selection(s, p$train, p$valid, vars)
    list(grid = s$grid, variants = s$variants, chosen = s$chosen, costs = s$costs)
  })
}
t0 <- elapsed()
blind_real <- blind_run(full)
permuted <- copy(full)
set.seed(seed)
permuted[an == test_year, grave := grave[sample.int(.N)]]
stopifnot(!identical(permuted$grave, full$grave), identical(permuted[an != test_year], full[an != test_year]))
blind_permuted <- blind_run(permuted)
rm(permuted)
same <- function(a, b) {
  keep <- c("grid", "variants", "chosen")
  identical(lapply(a, `[`, keep), lapply(b, `[`, keep))
}
stopifnot(same(blind_real, blind_permuted))
cat(sprintf(paste0("\nBlind check: with the %d labels permuted, same AUC on %d, settings, rebalancing and ",
                   "thresholds (%d fits per run, %g %% of the training accidents, %.1f s).\n"),
            test_year, valid_year, sum(vapply(blind_real, function(b) nrow(b$grid), integer(1))),
            100 * blind_fraction, elapsed() - t0))
rm(blind_permuted)
invisible(gc(full = TRUE))

# --- Selection ---------------------------------------------------------------------

invisible(gc(reset = TRUE, full = TRUE))
t0 <- elapsed()
parts <- year_parts(full, train_fraction, seed)
check_parts(full, parts)
cat(sprintf("\n## Parts: training %d users (%g %% of the %d-%d accidents), validation %d (%d not read)\n\n",
            length(parts$train$y), 100 * train_fraction, min(train_years), max(train_years),
            length(parts$valid$y), test_year))
cat(sprintf("Share of grave: training %.2f %%, validation %.2f %%\n",
            100 * mean(parts$train$y), 100 * mean(parts$valid$y)))
naive <- naive_metrics(parts$valid$y)
cat(sprintf("Naive references on %d: constant score AUC %.2f; all positive: precision %.4f, recall %.0f, F1 %.4f\n",
            valid_year, naive$constant_auc, naive$all_positive$metrics[["precision"]],
            naive$all_positive$metrics[["recall"]], naive$all_positive$metrics[["f1"]]))

test <- test_part(full)
importance_rows_test <- sample_rows(length(test$y), importance_size, groups = test$accident, seed = seed)
cat(sprintf("Test %d: %d users, %d accidents, share of grave %.2f %%, read after the selection of each specification\n",
            test_year, length(test$y), uniqueN(test$accident), 100 * mean(test$y)))

selection <- list()
results <- list()
probabilities <- list()
selection_seconds <- 0
for (spec in c("main", "ablation")) {
  vars <- if (spec == "main") model_vars_main else model_vars_ablation
  check_model_vars(vars)
  t_spec <- elapsed()
  sel <- select_models(parts$train, parts$valid, vars, settings_grid, seed, threads)
  check_selection(sel, parts$train, parts$valid, vars)
  selection_seconds <- selection_seconds + elapsed() - t_spec
  label <- sprintf("%s specification (%d variables)", spec, length(vars))
  cat(sprintf("\n## AUC on %d of each setting, %s\n\n", valid_year, label))
  show(merge(sel$grid, sel$costs, by = c("model", "balance", "setting"), sort = FALSE)[
    , .(model, balance, setting, auc = round(auc, 4), aliased, non_estimable,
        fit_s = round(fit_seconds, 1), predict_s = round(predict_seconds, 1), forest_mb = round(forest_mb))])
  cat(sprintf("\n## Best setting of each rebalancing on %d and chosen variant, %s\n\n", valid_year, label))
  show(variants_table(sel)[, lapply(.SD, function(v) if (is.double(v)) round(v, 4) else v)])

  # The test year, once the choices of this specification are fixed.
  t_eval <- elapsed()
  evaluated <- evaluate_variants(sel, test, vars)
  tested <- test_table(sel, evaluated)
  cat(sprintf("\n## Evaluation on %d at the thresholds of %d, %s\n\n", test_year, valid_year, label))
  show(tested[, lapply(.SD, function(v) if (is.double(v)) round(v, 4) else v)])
  logistic <- logistic_table(sel)
  cat(sprintf("\nLogistic regression (%s): 10 largest standardized coefficients, odds ratios without rebalancing\n\n",
              sel$chosen$glm$balance))
  show(head(as.data.table(logistic)[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)], 10))
  cat(sprintf("\n## Permutation importance on %d users of %d (%d accidents), %d permutations\n\n",
              length(importance_rows_test), test_year, uniqueN(test$accident[importance_rows_test]),
              importance_reps))
  importance <- test_importances(sel, test, vars, importance_rows_test, importance_reps, seed)
  for (type in names(importance)) {
    cat(sprintf("\n%s, 8 largest:\n", type))
    show(head(as.data.table(importance[[type]])[, .(variable, importance = round(importance, 4),
                                                    sd = round(sd, 4))], 8))
  }
  cat(sprintf("\nEvaluation of the %s specification: %.1f s\n", spec, elapsed() - t_eval))

  for (type in names(evaluated)) {
    for (b in names(evaluated[[type]])) {
      probabilities[[paste(spec, type, b)]] <- evaluated[[type]][[b]]$prob
      evaluated[[type]][[b]]$prob <- NULL
    }
  }
  results[[spec]] <- list(
    vars = vars, grid = as.data.frame(sel$grid), validation = as.data.frame(variants_table(sel)),
    test = as.data.frame(tested),
    roc = lapply(evaluated, function(e) lapply(e, `[[`, "roc")),
    logistic = logistic, importance = importance,
    recipe = list(impute = sel$recipe$params$impute, rare_threshold = rare_threshold),
    n_coefficients = length(stats::coef(sel$models$glm[[sel$chosen$glm$balance]]$fit))
  )
  # The models are dropped before the next specification: on the whole
  # training set a forest weighs hundreds of MB.
  selection[[spec]] <- c(sel[c("grid", "variants", "chosen", "costs")], list(recipe = sel$recipe["params"]))
  rm(sel, evaluated)
  invisible(gc(full = TRUE))
}
run_seconds <- selection_seconds
mem <- gc(full = TRUE)

cat(sprintf("\nTraining median of vma, imputed: %s\n", selection$main$recipe$params$impute$vma))
# No aliased coefficient in the logistic regressions of the main specification.
stopifnot(all(selection$main$grid[model == "glm", aliased] == 0))
cat(sprintf("Main logistic regressions: %d coefficients, none aliased; %d non estimable rows on %d.\n",
            results$main$n_coefficients, selection$main$chosen$glm$non_estimable, valid_year))
cat(sprintf("Ablation logistic regressions: %s aliased coefficients.\n",
            paste(selection$ablation$grid[model == "glm", aliased], collapse = " and ")))

# --- Intervals on 2024 ---------------------------------------------------------------

# The weighted AUC with weights 1 is the AUC of pROC, and with the weights of
# one draw the AUC of the rows repeated (checked on two scores).
check_names <- names(probabilities)[1:2]
w_check <- with_seed(seed + 1, tabulate(sample.int(uniqueN(test$accident), uniqueN(test$accident),
                                                    replace = TRUE), uniqueN(test$accident)))
w_check <- w_check[match(test$accident, unique(test$accident))]
for (k in check_names) {
  r <- score_sort(probabilities[[k]])
  repeated <- rep(seq_along(test$y), w_check)
  stopifnot(
    isTRUE(all.equal(weighted_auc(test$y, r, rep(1, length(test$y))),
                     roc_curve(test$y, probabilities[[k]])$auc, tolerance = 1e-12)),
    isTRUE(all.equal(weighted_auc(test$y, r, w_check),
                     roc_curve(test$y[repeated], probabilities[[k]][repeated])$auc, tolerance = 1e-12))
  )
}

t0 <- elapsed()
draws <- cluster_bootstrap_auc(test$y, probabilities, test$accident, boot_draws, seed)
estimate <- vapply(probabilities, function(p) roc_curve(test$y, p)$auc, numeric(1))
chosen_key <- function(spec, type) paste(spec, type, selection[[spec]]$chosen[[type]]$balance)
types <- names(settings_grid)
differences <- c(
  # Between models, chosen variants.
  unlist(lapply(names(results), function(spec) {
    lapply(utils::combn(c("ranger", "glm", "rpart"), 2, simplify = FALSE),
           function(p) c(chosen_key(spec, p[1]), chosen_key(spec, p[2])))
  }), recursive = FALSE),
  # Class weights against no rebalancing, best setting of each.
  unlist(lapply(names(results), function(spec) {
    lapply(types, function(type) paste(spec, type, c("weights", "none")))
  }), recursive = FALSE),
  # Ablation against the main specification, chosen variants.
  lapply(types, function(type) c(chosen_key("ablation", type), chosen_key("main", type)))
)
intervals <- interval_table(estimate, draws, differences)
cat(sprintf("\n## AUC on %d and differences, 95 %% percentile intervals, bootstrap by accident (B = %d, %.1f s)\n\n",
            test_year, boot_draws, elapsed() - t0))
show(intervals[, .(quantity, estimate = round(estimate, 4), lower = round(lower, 4), upper = round(upper, 4))])

# --- Results ---------------------------------------------------------------------------

naive_test <- naive_metrics(test$y)
final_models <- list(
  meta = list(
    train_years = train_years, valid_year = valid_year, test_year = test_year,
    train_fraction = train_fraction, seed = seed, threshold_grid_step = 0.001,
    n_train = length(parts$train$y), n_valid = length(parts$valid$y), n_test = length(test$y),
    n_test_accidents = uniqueN(test$accident),
    prevalence = c(train = mean(parts$train$y), valid = mean(parts$valid$y), test = mean(test$y)),
    settings_grid = settings_grid, balance_choices = balance_choices, ranger_trees = RANGER_TREES,
    boot_draws = boot_draws, importance_users = length(importance_rows_test),
    importance_reps = importance_reps,
    versions = c(R = R.version.string, ranger = as.character(utils::packageVersion("ranger")),
                 rpart = as.character(utils::packageVersion("rpart")),
                 pROC = as.character(utils::packageVersion("pROC")))
  ),
  leak = as.data.frame(leak),
  naive_test = list(constant_auc = naive_test$constant_auc,
                    all_positive = naive_test$all_positive$metrics),
  specs = results,
  intervals = as.data.frame(intervals)
)
dir.create(dirname(results_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(final_models, results_path)
cat(sprintf("\nNaive references on %d: constant score AUC %.2f; all positive: precision %.4f, F1 %.4f\n",
            test_year, naive_test$constant_auc, naive_test$all_positive$metrics[["precision"]],
            naive_test$all_positive$metrics[["f1"]]))
cat(sprintf("Results written to %s (%.2f MB on disk, %.2f MB in memory)\n", results_path,
            file.size(results_path) / 1024^2, as.numeric(object.size(final_models)) / 1024^2))

# --- Time and memory ---------------------------------------------------------------

both <- function(runs) rbindlist(lapply(names(runs), function(s) runs[[s]]$costs[, spec := s]))
measured <- both(selection)[, .(fit_s = sum(fit_seconds), predict_s = sum(predict_seconds)), by = model]
cat(sprintf("\n## Time and memory\n\nSelection on %g %% of the training accidents, both specifications (%d fits): %.1f s\n",
            100 * train_fraction, nrow(both(selection)), run_seconds))
cat(sprintf("\nSeconds by model at %g %% (fit, and prediction of %d):\n\n", 100 * train_fraction, valid_year))
show(measured[, .(model, fit_s = round(fit_s, 1), predict_s = round(predict_s, 1))])
cat("\nForest size by node size (MB, object.size: a single object without environment):\n\n")
show(both(selection)[model == "ranger", .(min_mb = round(min(forest_mb)), max_mb = round(max(forest_mb))),
                     by = .(spec, node = sub(".*min.node.size = ", "", setting))])

# Estimate for the whole training set, on a run below 100 %: for each fit of
# the blind grid, the exponent b of t = a n^b between the blind sample and
# train_fraction (same specification, model, rebalancing and setting); the
# median exponent of each model scales the time measured at train_fraction.
if (train_fraction > blind_fraction && train_fraction < 1) {
  matched <- merge(both(blind_real), both(selection), by = c("spec", "model", "balance", "setting"),
                   suffixes = c("_small", "_large"))
  ratio <- log(train_fraction / blind_fraction)
  exponents <- matched[, .(fit_exponent = stats::median(log(fit_seconds_large / fit_seconds_small) / ratio),
                           predict_exponent = stats::median(log(predict_seconds_large / predict_seconds_small) / ratio)),
                       by = model]
  times <- merge(measured, exponents, by = "model")
  times[, full_s := fit_s * (1 / train_fraction)^fit_exponent + predict_s * (1 / train_fraction)^predict_exponent]
  cat(sprintf("\nExponents of n measured between %g %% and %g %%, estimate for 100 %%:\n\n",
              100 * blind_fraction, 100 * train_fraction))
  show(times[, .(model, fit_exponent = round(fit_exponent, 2), predict_exponent = round(predict_exponent, 2),
                 full_s = round(full_s))])
  cat(sprintf("\nEstimated selection on the whole training set: %.0f min\n", sum(times$full_s) / 60))
  # Forest size, linear in the number of leaves: estimate for 100 % by node size.
  size <- matched[model == "ranger", .(spec, balance, setting, mb_small = forest_mb_small, mb_large = forest_mb_large,
                                       exponent = log(forest_mb_large / forest_mb_small) / ratio)]
  size[, mb_full := mb_large * (1 / train_fraction)^exponent]
  cat("\nForest size (MB) and estimate for 100 %:\n\n")
  show(size[, .(spec, balance, setting, mb_small = round(mb_small, 1), mb_large = round(mb_large, 1),
                exponent = round(exponent, 2), mb_full = round(mb_full))])
}
cat(sprintf("\nR heap peak of the selection: %.0f MB (gc max used, Ncells and Vcells; ranger grows its trees outside the R heap)\n",
            sum(mem[, ncol(mem)])))
cat(sprintf("Total time of the script: %.1f s\n", elapsed() - started_all))
