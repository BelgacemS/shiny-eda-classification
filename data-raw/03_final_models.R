# Final BAAC models (PLAN.md, step E12a; the full run and the 2024 evaluation
# are step E12b).
# Usage, from the project root, after data-raw/02_prepare_baac.R:
#   Rscript data-raw/03_final_models.R
# Environment variables: TRAIN_FRACTION (share of the 2019-2022 training
# accidents used, 0.1 by default), RANGER_THREADS (4 by default; a forest is
# the same whatever the number of threads, its seed being fixed).
#
# Split by year: training 2019-2022, validation 2023, test 2024. Settings,
# rebalancing and threshold are chosen on 2023 by select_models(), which only
# receives the training and validation sets. The 2024 labels are never read
# here; a check permutes them and expects the same choices.

library(data.table)
for (f in c("fct_stats_base.R", "fct_recipe.R", "fct_split.R", "fct_balance.R",
            "fct_metrics.R", "fct_models.R")) {
  source(file.path("R", f), encoding = "UTF-8")
}
source(file.path("data-raw", "model_vars.R"), encoding = "UTF-8")
options(width = 200)

full_path <- file.path("data-raw", "out", "accidents_full.rds")
stability_path <- file.path("data-raw", "out", "stability_2019_2023.csv")

train_fraction <- as.numeric(Sys.getenv("TRAIN_FRACTION", "0.1"))
blind_fraction <- 0.02  # training sample of the blind check
threads <- as.integer(Sys.getenv("RANGER_THREADS", "4"))
seed <- 20261115
train_years <- 2019:2022
valid_year <- 2023
test_year <- 2024
review_auc <- 0.8  # univariate AUC above which a variable is reviewed before being kept

# Recipe of the final models, the defaults of the app (PLAN.md, E12c): NA
# imputed by the training median (quanti) or mode (quali), the frequent NA of
# trajet and circ being already a level (model_vars.R); levels under 0.5 % of
# the non-missing training values go to
# "Autre", on the variables with more than two levels (grouping a binary
# variable would only rename its rare level). No scaling or dummy coding:
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
# without rebalancing and with class weights.
settings_grid <- list(
  glm = list(list()),
  ranger = apply(expand.grid(mtry = c(3, 5, 8), min.node.size = c(10, 50, 200)), 1, as.list),
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

# Training, validation and test parts by year: list(x, y) each, x the
# whitelisted predictors, y the 0/1 target. The training set is a sample of
# whole accidents, `fraction` of its rows.
year_parts <- function(full, fraction, seed) {
  part <- function(rows) {
    list(x = as.data.frame(full[rows, ..model_whitelist]), y = as.integer(full$grave[rows] == "oui"))
  }
  train_rows <- which(full$an %in% train_years)
  keep <- sample_rows(length(train_rows), round(fraction * length(train_rows)),
                      groups = full$Num_Acc[train_rows], seed = seed)
  list(train = part(train_rows[keep]), valid = part(which(full$an == valid_year)),
       test = part(which(full$an == test_year)))
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

# For each model type, the settings and the rebalancing of highest AUC on the
# validation set (first in grid order on a tie, "none" before "weights"), then
# the threshold of maximal F1 on that set. The recipe is learned on the
# training set only. Receives the training and validation parts, never the
# test set. Returns the grid (AUC of every fit), the choice per type, the
# chosen models, the fitted recipe, and the time and size of every fit.
select_models <- function(train, valid, vars, grid, seed, threads) {
  x_train <- train$x[vars]
  fitted <- fit_recipe(final_recipe(x_train), x_train, var_types_of(x_train))
  y_train <- train$y[fitted$kept_rows]
  x_valid <- apply_recipe(fitted, valid$x[vars])
  rows <- list()
  costs <- list()
  chosen <- list()
  models <- list()
  for (type in names(grid)) {
    best <- NULL
    for (balance in balance_choices) {
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
          best <- list(auc = auc, balance = balance, setting = setting, model = model, prob = prob)
        }
        rm(model)
      }
    }
    threshold <- best_f1_threshold(valid$y, best$prob)
    at <- threshold_metrics(valid$y, best$prob, threshold)
    chosen[[type]] <- list(balance = best$balance, setting = best$setting, auc = best$auc,
                           threshold = threshold, metrics = at$metrics,
                           confusion = unclass(at$confusion),
                           non_estimable = length(attr(best$prob, "non_estimable")))
    models[[type]] <- best$model
    rm(best)
  }
  list(grid = rbindlist(rows), chosen = chosen, models = models, recipe = fitted,
       costs = rbindlist(costs))
}

chosen_table <- function(sel) {
  rbindlist(lapply(names(sel$chosen), function(type) {
    ch <- sel$chosen[[type]]
    data.table(model = type, balance = ch$balance, setting = setting_label(ch$setting),
               auc = round(ch$auc, 4), threshold = ch$threshold,
               precision = round(ch$metrics[["precision"]], 4),
               recall = round(ch$metrics[["recall"]], 4), f1 = round(ch$metrics[["f1"]], 4),
               non_estimable = ch$non_estimable)
  }))
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
cat("\nWhitelist checked: no identifier, year or target, no catv next to catv_groupe.\n")

# --- Leak control on 2023 --------------------------------------------------------

# On the whole 2019-2022 training set and the original variables: univariate
# AUC (learned on training, measured on 2023) and NA share by class on 2023.
parts_all <- year_parts(full, 1, seed)
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
stopifnot(!any(leak$auc > review_auc, na.rm = TRUE))
rm(parts_all)
invisible(gc(full = TRUE))

# --- Blind check -------------------------------------------------------------------

# The selection with the 2024 labels permuted gives the same grid and choices.
blind_run <- function(data) {
  p <- year_parts(data, blind_fraction, seed)
  lapply(list(main = model_vars_main, ablation = model_vars_ablation), function(vars) {
    s <- select_models(p$train, p$valid, vars, blind_grid, seed, threads)
    list(grid = s$grid, chosen = s$chosen, costs = s$costs)
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
same <- function(a, b) identical(lapply(a, `[`, c("grid", "chosen")), lapply(b, `[`, c("grid", "chosen")))
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
cat(sprintf("\n## Parts: training %d users (%g %% of the %d-%d accidents), validation %d, test %d\n\n",
            length(parts$train$y), 100 * train_fraction, min(train_years), max(train_years),
            length(parts$valid$y), length(parts$test$y)))
cat(sprintf("Share of grave: training %.2f %%, validation %.2f %% (test labels not read)\n",
            100 * mean(parts$train$y), 100 * mean(parts$valid$y)))
naive <- naive_metrics(parts$valid$y)
cat(sprintf("Naive references on %d: constant score AUC %.2f; all positive: precision %.4f, recall %.0f, F1 %.4f\n",
            valid_year, naive$constant_auc, naive$all_positive$metrics[["precision"]],
            naive$all_positive$metrics[["recall"]], naive$all_positive$metrics[["f1"]]))

selection <- list()
for (spec in c("main", "ablation")) {
  vars <- if (spec == "main") model_vars_main else model_vars_ablation
  check_model_vars(vars)
  sel <- select_models(parts$train, parts$valid, vars, settings_grid, seed, threads)
  label <- sprintf("%s specification (%d variables)", spec, length(vars))
  cat(sprintf("\n## AUC on %d of each setting, %s\n\n", valid_year, label))
  show(merge(sel$grid, sel$costs, by = c("model", "balance", "setting"), sort = FALSE)[
    , .(model, balance, setting, auc = round(auc, 4), aliased, non_estimable,
        fit_s = round(fit_seconds, 1), predict_s = round(predict_seconds, 1), forest_mb = round(forest_mb))])
  cat(sprintf("\n## Chosen on %d, %s\n\n", valid_year, label))
  show(chosen_table(sel))
  selection[[spec]] <- sel
  rm(sel)
}
run_seconds <- elapsed() - t0
mem <- gc(full = TRUE)

cat(sprintf("\nTraining median of vma, imputed: %s\n", selection$main$recipe$params$impute$vma))
# No aliased coefficient in the logistic regressions of the main specification.
stopifnot(all(selection$main$grid[model == "glm", aliased] == 0))
cat(sprintf("Main logistic regressions: %d coefficients, none aliased; %d non estimable rows on %d.\n",
            length(stats::coef(selection$main$models$glm$fit)), selection$main$chosen$glm$non_estimable,
            valid_year))
cat(sprintf("Ablation logistic regressions: %s aliased coefficients.\n",
            paste(selection$ablation$grid[model == "glm", aliased], collapse = " and ")))

# --- Time and memory ---------------------------------------------------------------

# Estimate for the whole training set: for each fit of the blind grid, the
# exponent b of t = a n^b between the blind sample and train_fraction (same
# specification, model, rebalancing and setting); the median exponent of each
# model scales the time measured at train_fraction.
both <- function(runs) rbindlist(lapply(names(runs), function(s) runs[[s]]$costs[, spec := s]))
matched <- merge(both(blind_real), both(selection), by = c("spec", "model", "balance", "setting"),
                 suffixes = c("_small", "_large"))
ratio <- log(train_fraction / blind_fraction)
exponents <- matched[, .(fit = stats::median(log(fit_seconds_large / fit_seconds_small) / ratio),
                         predict = stats::median(log(predict_seconds_large / predict_seconds_small) / ratio)),
                     by = model]
measured <- both(selection)[, .(fit = sum(fit_seconds), predict = sum(predict_seconds)), by = model]
times <- merge(measured, exponents, by = "model", suffixes = c("_s", "_exponent"))
times[, full_s := fit_s * (1 / train_fraction)^fit_exponent + predict_s * (1 / train_fraction)^predict_exponent]
cat(sprintf("\n## Time and memory\n\nSelection on %g %% of the training accidents, both specifications (%d fits): %.1f s\n",
            100 * train_fraction, nrow(both(selection)), run_seconds))
cat(sprintf("Seconds by model at %g %%, exponents of n measured between %g %% and %g %%, estimate for 100 %%:\n\n",
            100 * train_fraction, 100 * blind_fraction, 100 * train_fraction))
show(times[, .(model, fit_s = round(fit_s, 1), predict_s = round(predict_s, 1),
               fit_exponent = round(fit_exponent, 2), predict_exponent = round(predict_exponent, 2),
               full_s = round(full_s))])
cat(sprintf("\nEstimated selection on the whole training set: %.0f min\n", sum(times$full_s) / 60))

# Forest size, linear in the number of leaves: estimate for 100 % by node size.
size <- matched[model == "ranger", .(spec, balance, setting, mb_small = forest_mb_small, mb_large = forest_mb_large,
                                     exponent = log(forest_mb_large / forest_mb_small) / ratio)]
size[, mb_full := mb_large * (1 / train_fraction)^exponent]
cat("\nForest size (MB, object.size: a single object without environment) and estimate for 100 %:\n\n")
show(size[, .(spec, balance, setting, mb_small = round(mb_small, 1), mb_large = round(mb_large, 1),
              exponent = round(exponent, 2), mb_full = round(mb_full))])
cat(sprintf("\nR heap peak of the selection: %.0f MB (gc max used, Ncells and Vcells; ranger grows its trees outside the R heap)\n",
            sum(mem[, ncol(mem)])))
cat(sprintf("Total time of the script: %.1f s\n", elapsed() - started_all))
