# Tab 5 (E13). Synthetic sample and results for CI, where data/ is absent;
# the claims written in the tab are checked on the real files when present.

fake_sample <- function() {
  levels_catv <- c("VL", "VU", "Vélo", "EDP", "2RM/3RM <= 50 cm3", "2RM/3RM 50 à 125 cm3",
                   "2RM/3RM > 125 cm3", "PL, tracteur routier")
  n <- 480
  i <- seq_len(n)
  grave <- ifelse(i %% 5 == 0 | (i %% 7 == 0 & i %% 2 == 0), "oui", "non")
  grav <- ifelse(grave == "oui", ifelse(i %% 35 == 0, "Tué", "Blessé hospitalisé"),
                 ifelse(i %% 2 == 0, "Indemne", "Blessé léger"))
  data.frame(
    Num_Acc = sprintf("A%03d", (i + 1) %/% 2),
    an = rep(2019:2024, length.out = n),
    grave = factor(grave, levels = c("non", "oui")),
    grav = factor(grav, levels = c("Indemne", "Tué", "Blessé hospitalisé", "Blessé léger")),
    catu = factor(rep(c("Conducteur", "Conducteur", "Passager", "Piéton"), length.out = n)),
    catv_groupe = factor(rep(levels_catv, length.out = n), levels = levels_catv),
    agg = factor(rep(c("Hors agglomération", "En agglomération", "En agglomération"), length.out = n)),
    vma = rep(c(30, 50, 80, 90, 110, 130, 50), length.out = n),
    lum = factor(rep(c("Plein jour", "Plein jour", "Nuit sans éclairage public",
                       "Nuit avec éclairage public allumé", "Crépuscule ou aube"), length.out = n)),
    atm = factor(rep(c("Normale", "Normale", "Pluie légère"), length.out = n)),
    catr = factor(rep(c("Route départementale", "Voie communale", "Autoroute", "Voie communale"), length.out = n)),
    secu_ceinture = factor(rep(c("oui", "non", "oui"), length.out = n)),
    secu_indetermine = factor(rep(c("non", "non", "non", "oui"), length.out = n)),
    age = 18 + (i * 7) %% 60,
    stringsAsFactors = FALSE
  )
}

fake_final <- function() {
  models <- c("glm", "ranger", "rpart")
  test <- data.frame(
    model = rep(models, each = 2), balance = rep(c("none", "weights"), 3), chosen = rep(c(FALSE, TRUE), 3),
    setting = c("-", "-", "mtry = 3, min.node.size = 50", "mtry = 5, min.node.size = 50", "cp = 3e-04", "cp = 0.001"),
    threshold = c(0.2, 0.6, 0.3, 0.55, 0.25, 0.5), auc = c(0.80, 0.81, 0.84, 0.845, 0.75, 0.78),
    precision = c(0.40, 0.41, 0.45, 0.46, 0.35, 0.36), recall = c(0.60, 0.61, 0.65, 0.66, 0.55, 0.70),
    f1 = c(0.48, 0.49, 0.53, 0.54, 0.43, 0.48), tp = c(60L, 61L, 65L, 66L, 55L, 70L),
    fp = c(90L, 88L, 80L, 77L, 102L, 124L), fn = c(40L, 39L, 35L, 34L, 45L, 30L),
    tn = c(310L, 312L, 320L, 323L, 298L, 276L), non_estimable = 0L
  )
  roc <- lapply(stats::setNames(models, models), function(m) {
    list(none = data.frame(fpr = c(0, 0.3, 1), tpr = c(0, 0.7, 1)),
         weights = data.frame(fpr = c(0, 0.25, 1), tpr = c(0, 0.72, 1)))
  })
  vars <- c("secu_ceinture", "manv", "situ", "agg", "catv_groupe", "age", "vma", "catr", "atm")
  importance <- lapply(stats::setNames(models, models), function(m) {
    data.frame(variable = vars, importance = seq(0.09, 0.01, length.out = length(vars)), sd = 0.001)
  })
  logistic <- data.frame(
    term = c("secu_ceinturenon", "catuPiéton", "aggHors agglomération", "catrAutoroute"),
    coefficient = c(1.5, 1.1, 0.7, -1.2), sd = c(0.5, 0.3, 0.5, 0.3), standardized = c(0.75, 0.33, 0.35, 0.36),
    odds_ratio = c(4.7, 2.3, 1.9, 0.32), lower = c(4.5, 2.2, 1.8, 0.31), upper = c(4.9, 2.4, 2.0, 0.34),
    se = 0.02, separation = FALSE
  )
  grid <- data.frame(model = test$model, balance = test$balance, setting = test$setting, auc = test$auc)
  spec <- function(name) {
    t <- test
    if (name == "ablation") t$auc <- t$auc + 0.02
    list(vars = c(vars, "catu"), grid = grid, test = t, roc = roc, logistic = logistic,
         importance = importance, recipe = list(rare_threshold = 0.005))
  }
  specs <- list(main = spec("main"), ablation = spec("ablation"))
  q <- character(0)
  est <- numeric(0)
  for (s in names(specs)) for (k in seq_len(nrow(test))) {
    q <- c(q, paste(s, test$model[k], test$balance[k]))
    est <- c(est, specs[[s]]$test$auc[k])
  }
  names(est) <- q
  add <- function(a, b) {
    q <<- c(q, paste(a, "-", b))
    est <<- c(est, est[[a]] - est[[b]])
  }
  for (s in names(specs)) {
    add(paste(s, "ranger weights"), paste(s, "glm weights"))
    add(paste(s, "ranger weights"), paste(s, "rpart weights"))
    add(paste(s, "glm weights"), paste(s, "rpart weights"))
  }
  for (s in names(specs)) for (m in models) add(paste(s, m, "weights"), paste(s, m, "none"))
  for (m in models) add(paste("ablation", m, "weights"), paste("main", m, "weights"))
  list(
    meta = list(train_years = 2019:2022, valid_year = 2023, test_year = 2024, n_train = 1000L, n_valid = 300L,
                n_test = 500L, n_test_accidents = 250L, boot_draws = 1000,
                settings_grid = list(glm = list(list()),
                                     ranger = list(list(mtry = 3, min.node.size = 50), list(mtry = 5, min.node.size = 50)),
                                     rpart = list(list(cp = 3e-4), list(cp = 1e-3))),
                ranger_trees = 300, importance_users = 200L, importance_reps = 3),
    naive_test = list(constant_auc = 0.5, all_positive = c(precision = 0.2, recall = 1, f1 = 0.333)),
    specs = specs,
    intervals = data.frame(quantity = q, estimate = unname(est), lower = unname(est) - 0.003,
                           upper = unname(est) + 0.003)
  )
}

test_that("the case study dataset is the one of tabs 1 and 2 after the replay steps", {
  sample <- fake_sample()
  shiny::testServer(mod_data_server, args = list(baac_sample = sample), {
    session$setInputs(baac = 1)
    session$setInputs(type_vars = CASE_STUDY_RETYPED, type_new = "quali", type_apply = 1)
    expect_identical(clean_dataset(dataset()), case_study_dataset(sample))
  })
  ds <- case_study_dataset(sample)
  expect_true(all(ds$var_types[CASE_STUDY_RETYPED] == "quali"))
  expect_equal(ds$var_types[["age"]], "quanti")
})

test_that("a profile gives the table and indices that tab 3 shows for the pair", {
  ds <- case_study_dataset(fake_sample())
  for (v in c("an", "catv_groupe", "vma")) {
    shiny::testServer(mod_explore_bi_server, args = list(dataset_clean = shiny::reactive(ds)), {
      session$setInputs(x = v, y = CASE_STUDY_TARGET)
      pr <- grave_profile(ds, v)
      expect_identical(cc()$tab, pr$tab)
      expect_identical(cc()$stats, pr$stats)
    })
  }
  shiny::testServer(mod_explore_bi_server, args = list(dataset_clean = shiny::reactive(ds)), {
    session$setInputs(target = CASE_STUDY_TARGET)
    tab3 <- associations()
    q3 <- q3_associations(ds)
    expect_equal(q3$Valeur, format_number(tab3$value[match(Q3_VARS, tab3$variable)], 3))
    expect_equal(q3$Indice[q3$Variable == "age"], "Rapport de corrélation c")
  })
})

test_that("grave_profile computes the share of grave by level", {
  ds <- list(data = data.frame(x = factor(c("a", "a", "b", "b", "b", NA)),
                               grave = factor(c("oui", "non", "oui", "oui", "non", "oui"))),
             var_types = c(x = "quali", grave = "quali"))
  pr <- grave_profile(ds, "x")
  expect_equal(unname(pr$share), c(50, 200 / 3))  # 1 of 2, 2 of 3; the NA row is left out
  expect_equal(pr$stats$n, 5)
  expect_equal(share_of(pr, "b"), 200 / 3)
  expect_error(share_of(pr, "c"))
  expect_equal(profile_table(pr)$`Part de graves`, c("50,0 %", "66,7 %"))
})

test_that("the models are read from the results without being recomputed", {
  final <- fake_final()
  expect_silent(check_final_results(final))
  expect_error(check_final_results(final[c("meta", "specs")]))
  spec <- final$specs$main
  expect_equal(chosen_balance(spec), c(glm = "weights", ranger = "weights", rpart = "weights"))
  ev <- model_evaluations(spec)
  row <- spec$test[spec$test$model == "rpart" & spec$test$balance == "weights", ]
  expect_equal(ev$rpart$threshold, row$threshold)
  expect_equal(ev$rpart$roc$auc, row$auc)
  expect_equal(ev$rpart$confusion, c(tp = row$tp, fp = row$fp, fn = row$fn, tn = row$tn))
  expect_equal(unname(ev$rpart$metrics), c(row$precision, row$recall, row$f1))
  none <- model_evaluations(spec, c(glm = "none"))
  expect_equal(names(none), "glm")
  expect_equal(none$glm$threshold, 0.2)

  expect_equal(interval_of(final, "main glm weights"), c(estimate = 0.81, lower = 0.807, upper = 0.813))
  expect_error(interval_of(final, "main glm"))
  expect_equal(format_interval(c(estimate = 0.8, lower = 0.79, upper = 0.81)), "0,800 [0,790 ; 0,810]")

  tt <- test_table(final, "main")
  expect_equal(tt$`AUC [IC 95 %]`[1], "0,810 [0,807 ; 0,813]")
  expect_equal(tt$`Seuil (2023)`[2], "0,550")
  expect_equal(tail(tt$F1, 1), "0,333")
  bt <- balance_table(final, "main")
  expect_equal(bt$`Écart [IC 95 %]`[bt$Modèle == MODEL_LABELS[["rpart"]]], "0,0300 [0,0270 ; 0,0330]")
  at <- ablation_table(final)
  expect_equal(at$`Écart [IC 95 %]`[1], "0,0200 [0,0170 ; 0,0230]")
  dt <- model_difference_table(final, "main")
  expect_equal(dt$`Écart d'AUC [IC 95 %]`[1], "0,0350 [0,0320 ; 0,0380]")
  expect_equal(odds_ratio_of(spec, "catuPiéton"), c(estimate = 2.3, lower = 2.2, upper = 2.4))
})

test_that("the replay removes every candidate outside the list, never the target or an id", {
  ds <- case_study_dataset(fake_sample())
  removed <- replay_removed(ds, c("catu", "age"))
  expect_false(any(c("grave", "Num_Acc", "catu", "age") %in% removed))
  expect_true(all(c("an", "grav", "vma") %in% removed))
})

test_that("the tab renders with the sample and the results", {
  shiny::testServer(mod_case_study_server,
                    args = list(baac_sample = fake_sample(), final_results = fake_final()), {
    expect_null(output$status)
    for (v in Q3_VARS) {
      session$setInputs(q3_var = v)
      for (o in c("q3_plot", "q3_detail", "q3_indices", "q3_replay")) expect_no_error(output[[o]])
    }
    for (o in c("approach", "severity_plot", "severity_text", "year_plot", "year_table", "year_text",
                "full_years", "catu_plot", "catu_text", "catv_plot", "catv_text", "q3_table", "q3_text",
                "test_table", "test_text", "roc_plot", "difference_table", "confusions", "balance_table",
                "balance_text", "ablation_table", "ablation_text", "importance_text", "importance_plot",
                "logistic_table", "logistic_text", "coherence_text", "model_replay", "limits")) {
      expect_no_error(output[[o]])
    }
    expect_match(output$test_text$html, "La forêt aléatoire a la meilleure AUC")
    expect_match(output$model_replay$html, "Num_Acc")
  })
})

test_that("each part says which file is missing", {
  shiny::testServer(mod_case_study_server, args = list(baac_sample = NULL, final_results = fake_final()), {
    expect_match(output$status$html, "accidents_sample.rds")
    expect_no_error(output$test_table)
    expect_error(output$year_table, "accidents_sample.rds")
  })
  shiny::testServer(mod_case_study_server, args = list(baac_sample = fake_sample(), final_results = NULL), {
    expect_match(output$status$html, "final_models.rds")
    expect_no_error(output$year_table)
    expect_error(output$test_table, "final_models.rds")
  })
})

# --- The claims written in the tab, on the real files -------------------------------

sample_path <- test_path("..", "..", "data", "accidents_sample.rds")
results_path <- test_path("..", "..", "data", "final_models.rds")
full_path <- test_path("..", "..", "data-raw", "out", "accidents_full.rds")

test_that("the descriptive claims hold on the deployed sample", {
  skip_if_not(file.exists(sample_path))
  ds <- case_study_dataset(readRDS(sample_path))
  pr <- function(v) grave_profile(ds, v)
  catu <- pr("catu")
  expect_gt(share_of(catu, "Piéton"), 1.5 * share_of(catu, "Conducteur"))
  catv <- pr("catv_groupe")
  expect_equal(names(which.max(catv$share)), "2RM/3RM > 125 cm3")
  expect_gt(min(catv$share[c("2RM/3RM <= 50 cm3", "2RM/3RM 50 à 125 cm3", "Vélo")]), share_of(catv, "VL"))
  agg <- pr("agg")
  expect_gt(share_of(agg, "Hors agglomération"), share_of(agg, "En agglomération"))
  catr <- pr("catr")
  expect_equal(names(which.max(catr$share)), "Route départementale")
  expect_lt(share_of(catr, "Autoroute"), share_of(catr, "Voie communale"))
  belt <- pr("secu_ceinture")
  expect_gt(share_of(belt, "non"), 2 * share_of(belt, "oui"))
  expect_true(all(ds$data$secu_ceinture[ds$data$catu == "Piéton"] == "non"))
  expect_true(all(ds$data$secu_ceinture[ds$data$secu_indetermine == "oui"] == "non"))
  vma <- pr("vma")$share[c("30", "50", "80", "90", "110", "130")]
  expect_true(is.unsorted(vma))
  lum <- pr("lum")
  expect_gt(share_of(lum, "Nuit sans éclairage public"), share_of(lum, "Plein jour"))
  light_area <- row_profiles(contingency_table(ds$data$lum, ds$data$agg))
  expect_gt(light_area["Nuit sans éclairage public", "Hors agglomération"], 0.5)
  expect_lt(pr("atm")$stats$cramer, 0.1)
  expect_lt(pr("an")$stats$cramer, 0.05)
  age <- age_profile(ds)
  expect_lt(age$c, 0.1)
  expect_gt(age$groups$mean[age$groups$level == "oui"], age$groups$mean[age$groups$level == "non"])
  sev <- severity_table(ds)
  expect_equal(sev$level, c("Indemne", "Tué", "Blessé hospitalisé", "Blessé léger"))
})

test_that("the yearly figures of the full table are those of the prepared table", {
  skip_if_not(file.exists(full_path))
  full <- readRDS(full_path)
  users <- as.vector(table(full$an))
  share <- round(100 * tapply(full$grave == "oui", full$an, mean), 2)
  expect_equal(users, FULL_TABLE_YEARS$users)
  expect_equal(as.vector(share), FULL_TABLE_YEARS$grave_pct)
  y <- FULL_TABLE_YEARS
  expect_gte(y$grave_pct[y$an == 2020], y$grave_pct[y$an == 2019])
})

test_that("the model claims hold on the final results", {
  skip_if_not(file.exists(results_path))
  final <- readRDS(results_path)
  check_final_results(final)
  spec <- final$specs$main
  ev <- model_evaluations(spec)
  auc <- vapply(ev, function(e) e$roc$auc, numeric(1))
  expect_equal(names(which.max(auc)), "ranger")
  expect_gt(auc[["glm"]], auc[["rpart"]])
  diff <- function(m) interval_of(final, paste("main", m, "weights - main", m, "none"))[["estimate"]]
  expect_lt(abs(diff("glm")), 0.001)
  expect_lt(abs(diff("ranger")), 0.001)
  expect_gt(diff("rpart"), 5 * max(abs(diff("glm")), abs(diff("ranger"))))
  expect_gt(interval_of(final, "main ranger weights - main glm weights")[["lower"]], 0)
  # Reference levels named in the text of the odds ratios.
  terms <- spec$logistic$term
  expect_true(all(c("secu_ceinturenon", "catuPiéton", "aggHors agglomération", "catrAutoroute") %in% terms))
  expect_false(any(c("secu_ceintureoui", "catuConducteur", "aggEn agglomération", "catrRoute départementale") %in% terms))
  forest <- spec$importance$ranger
  expect_gt(match("atm", forest$variable), 10)
  expect_true(all(vapply(spec$importance, function(d) d$variable[1], character(1)) == "secu_ceinture"))
  gains <- final$intervals$estimate[grepl("^ablation .* - main ", final$intervals$quantity)]
  expect_true(all(gains > 0))
})
