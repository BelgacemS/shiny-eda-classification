# Tab 5 (E13). Synthetic sample and results for CI, where data/ is absent;
# the claims written in the tab are checked on the real files when present.

# Random but fixed sample: the shares differ between levels, so that a swapped
# level or argument changes the text.
fake_sample <- function(n = 3000) {
  set.seed(20261118)
  levels_catv <- c("VL", "VU", "Vélo", "EDP", "2RM/3RM <= 50 cm3", "2RM/3RM 50 à 125 cm3",
                   "2RM/3RM > 125 cm3", "PL, tracteur routier")
  pick <- function(levels, prob = NULL) factor(sample(levels, n, TRUE, prob), levels = levels)
  catv <- pick(levels_catv)
  catu <- pick(c("Conducteur", "Passager", "Piéton"), c(0.7, 0.2, 0.1))
  agg <- pick(c("Hors agglomération", "En agglomération"))
  lum <- pick(c("Plein jour", "Crépuscule ou aube", "Nuit sans éclairage public",
                "Nuit avec éclairage public allumé"), c(0.6, 0.1, 0.15, 0.15))
  atm <- pick(c("Normale", "Pluie légère", "Brouillard, fumée", "Vent fort, tempête"), c(0.7, 0.2, 0.05, 0.05))
  catr <- pick(c("Autoroute", "Route départementale", "Voie communale"))
  belt <- ifelse(catu == "Piéton", "non", sample(c("oui", "non"), n, TRUE, c(0.7, 0.3)))
  unknown <- ifelse(belt == "non" & stats::runif(n) < 0.3, "oui", "non")
  vma <- sample(c(30, 50, 80, 90, 110, 130), n, TRUE)
  age <- sample(18:90, n, TRUE)
  an <- sample(2019:2024, n, TRUE)
  lin <- -2 + 0.8 * (catu == "Piéton") + 0.3 * (catu == "Passager") + 0.5 * (agg == "Hors agglomération") +
    0.6 * (belt == "non") + 0.05 * as.integer(catv) + 0.2 * (lum == "Nuit sans éclairage public") +
    0.01 * (age - 50) + 0.002 * vma + 0.05 * (an - 2019)
  grave <- ifelse(stats::runif(n) < stats::plogis(lin), "oui", "non")
  grav <- ifelse(grave == "oui", ifelse(stats::runif(n) < 0.15, "Tué", "Blessé hospitalisé"),
                 ifelse(stats::runif(n) < 0.5, "Indemne", "Blessé léger"))
  data.frame(
    Num_Acc = sprintf("A%04d", ceiling(seq_len(n) / 2)), an = an,
    grave = factor(grave, levels = c("non", "oui")),
    grav = factor(grav, levels = c("Indemne", "Tué", "Blessé hospitalisé", "Blessé léger")),
    catu = catu, catv_groupe = catv, agg = agg, vma = vma, lum = lum, atm = atm, catr = catr,
    secu_ceinture = factor(belt), secu_indetermine = factor(unknown), age = age,
    col = pick(c("Frontale", "Par le côté")), choc = pick(c("Avant", "Arrière")),
    obs = pick(c("Aucun", "Arbre")), obsm = pick(c("Aucun", "Piéton")),
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
  # A different order for the logistic regression than for the forest and the tree.
  orders <- list(glm = c(1, 5, 7, 2, 3, 4, 6, 8, 9), ranger = seq_along(vars), rpart = seq_along(vars))
  importance <- lapply(stats::setNames(models, models), function(m) {
    data.frame(variable = vars[orders[[m]]], importance = seq(0.09, 0.01, length.out = length(vars)), sd = 0.001)
  })
  logistic <- data.frame(
    term = c("secu_ceinturenon", "catuPiéton", "aggHors agglomération", "catrAutoroute"),
    coefficient = c(1.5, 1.1, 0.7, -1.2), sd = c(0.5, 0.3, 0.5, 0.3), standardized = c(0.75, 0.33, 0.35, 0.36),
    odds_ratio = c(4.7, 2.3, 1.9, 0.32), lower = c(4.5, 2.2, 1.8, 0.31), upper = c(4.9, 2.4, 2.0, 0.34),
    se = 0.02, separation = FALSE
  )
  grid <- data.frame(model = test$model, balance = test$balance, setting = test$setting, auc = test$auc)
  crash <- c("col", "choc", "obs", "obsm")
  spec <- function(name) {
    t <- test
    if (name == "ablation") t$auc <- t$auc + rep(c(0.01, 0.02, 0.03), each = 2)
    list(vars = c(vars, "catu", if (name == "ablation") crash), grid = grid, test = t, roc = roc,
         logistic = logistic, importance = importance, recipe = list(rare_threshold = 0.005))
  }
  specs <- list(main = spec("main"), ablation = spec("ablation"))
  # In the ablation, the logistic regression without rebalancing is chosen, so
  # that each specification has its own chosen variants.
  specs$ablation$test$chosen <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
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
  add("ablation glm none", "main glm weights")
  for (m in c("ranger", "rpart")) add(paste("ablation", m, "weights"), paste("main", m, "weights"))
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
  expect_equal(profile_table(pr)$Graves, c("1", "2"))
  expect_equal(profile_table(pr)$Usagers, c("2", "3"))
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
  expect_equal(at$`Écart [IC 95 %]`[1], "0,0000 [-0,0030 ; 0,0030]")
  expect_equal(at$`AUC principale`[1], "0,810 [0,807 ; 0,813]")
  expect_equal(at$`AUC avec le choc`[1], "0,810 [0,807 ; 0,813]")
  expect_equal(at$`AUC avec le choc`[2], "0,865 [0,862 ; 0,868]")
  expect_equal(ev$glm$roc$fpr, spec$roc$glm$weights$fpr)
  expect_equal(tt$Precision[nrow(tt)], "0,200")
  expect_equal(tt$Recall[nrow(tt)], "1,000")
  expect_equal(model_difference_table(final, "main")$Écart[1], "Forêt aléatoire moins régression logistique")
  glm_row <- bt[bt$Modèle == MODEL_LABELS[["glm"]], ]
  expect_equal(unlist(glm_row[c("AUC sans", "AUC pondérée", "Seuil sans", "Seuil pondéré", "F1 sans", "F1 pondéré")],
                      use.names = FALSE), c("0,800", "0,810", "0,200", "0,600", "0,480", "0,490"))
  expect_equal(crash_vars(final), c("col", "choc", "obs", "obsm"))
  expect_equal(enumerate_fr(c("a", "b", "c")), "a, b et c")
  expect_equal(enumerate_fr("a"), "a")
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
                "logistic_table", "logistic_text", "coherence_text", "model_replay", "limits", "conclusions")) {
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
    expect_match(output$limits$html, "Hooker et al., 2021")
    expect_error(output$conclusions, "final_models.rds")
  })
})

# --- The sentences of the tab, against values computed here without the tab's functions ---

plain <- function(x) {
  html <- if (is.list(x)) x$html else x
  gsub("\\s+", " ", gsub("<[^>]+>", " ", html))
}
expect_has <- function(text, fragment) {
  expect(grepl(fragment, text, fixed = TRUE), sprintf("missing: %s\nin: %s", fragment, substr(text, 1, 600)))
}
num <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ",")
share_by <- function(x, sample) 100 * tapply(sample$grave == "oui", x, mean)

test_that("the descriptive sentences give the right figures", {
  s <- fake_sample()
  shiny::testServer(mod_case_study_server, args = list(baac_sample = s, final_results = fake_final()), {
    f <- 100 * table(s$grav) / nrow(s)
    t1 <- plain(output$severity_text)
    expect_has(t1, sprintf("%s %% des usagers sont indemnes, %s %% blessés légers, %s %% blessés hospitalisés et %s %% tués : %s %% sont graves",
                           num(f[["Indemne"]]), num(f[["Blessé léger"]]), num(f[["Blessé hospitalisé"]]),
                           num(f[["Tué"]]), num(f[["Blessé hospitalisé"]] + f[["Tué"]])))

    by_year <- share_by(s$an, s)
    t2 <- plain(output$year_text)
    expect_has(t2, sprintf("va de %s %% (%s) à %s %% (%s)", num(min(by_year)), names(by_year)[which.min(by_year)],
                           num(max(by_year)), names(by_year)[which.max(by_year)]))
    expect_has(t2, sprintf("baisse de %s %%", num(100 * (1 - 99169 / 126352))))
    expect_has(t2, "(18,29 % contre 18,09 %)")

    u <- share_by(s$catu, s)
    expect_has(plain(output$catu_text), sprintf("Un piéton est grave dans %s %% des cas, contre %s %% pour un conducteur et %s %% pour un passager",
                                                num(u[["Piéton"]]), num(u[["Conducteur"]]), num(u[["Passager"]])))

    v <- share_by(s$catv_groupe, s)
    t3 <- plain(output$catv_text)
    expect_has(t3, sprintf("plus souvent graves (%s %%), devant ceux de 50 cm³ ou moins (%s %%), de 50 à 125 cm³ (%s %%) et les cyclistes (%s %%)",
                           num(v[["2RM/3RM > 125 cm3"]]), num(v[["2RM/3RM <= 50 cm3"]]),
                           num(v[["2RM/3RM 50 à 125 cm3"]]), num(v[["Vélo"]])))
    expect_has(t3, sprintf("(EDP) : %s %%", num(v[["EDP"]])))
    expect_has(t3, sprintf("graves dans %s %% des cas, ceux d'un poids lourd dans %s %%", num(v[["VL"]]),
                           num(v[["PL, tracteur routier"]])))
    expect_has(t3, sprintf("et %s piétons heurtés par une voiture, sur %s usagers",
                           num(sum(s$catv_groupe == "VL" & s$catu == "Piéton"), 0), num(sum(s$catv_groupe == "VL"), 0)))

    t4 <- plain(output$q3_text)
    b <- share_by(s$secu_ceinture, s)
    no_belt <- s$secu_ceinture == "non"
    expect_has(t4, sprintf("Sans ceinture, %s %% de graves, contre %s %% avec", num(b[["non"]]), num(b[["oui"]])))
    expect_has(t4, sprintf("piétons (%s des %s usagers à « non »)", num(sum(no_belt & s$catu == "Piéton"), 0),
                           num(sum(no_belt), 0)))
    expect_has(t4, sprintf("équipements indéterminés (%s)", num(sum(no_belt & s$secu_indetermine == "oui"), 0)))
    expect_has(t4, "odds ratio de 4,7")
    a <- share_by(s$agg, s)
    r <- share_by(s$catr, s)
    expect_has(t4, sprintf("Hors agglomération, %s %% de graves, contre %s %% en agglomération", num(a[["Hors agglomération"]]),
                           num(a[["En agglomération"]])))
    expect_has(t4, sprintf("départementales sont les plus touchées (%s %%), les autoroutes parmi les moins (%s %%)",
                           num(r[["Route départementale"]]), num(r[["Autoroute"]])))
    expect_has(t4, sprintf("voies communales : %s %%", num(r[["Voie communale"]])))
    m <- share_by(s$vma, s)
    expect_has(t4, sprintf("%s %% à 30 km/h, %s %% à 50, %s %% à 80, %s %% à 90, %s %% à 110 et %s %% à 130",
                           num(m[["30"]]), num(m[["50"]]), num(m[["80"]]), num(m[["90"]]), num(m[["110"]]), num(m[["130"]])))
    l <- share_by(s$lum, s)
    night <- s$lum == "Nuit sans éclairage public"
    expect_has(t4, sprintf("De nuit sans éclairage public, %s %% de graves, contre %s %% en plein jour et %s %% de nuit avec",
                           num(l[["Nuit sans éclairage public"]]), num(l[["Plein jour"]]),
                           num(l[["Nuit avec éclairage public allumé"]])))
    expect_has(t4, sprintf("hors agglomération (%s %% des cas)", num(100 * mean(s$agg[night] == "Hors agglomération"))))
    w <- share_by(s$atm, s)
    expect_has(t4, sprintf("%s %% de graves par temps normal, %s %% sous pluie légère. Brouillard (%s %%) et vent fort (%s %%)",
                           num(w[["Normale"]]), num(w[["Pluie légère"]]), num(w[["Brouillard, fumée"]]),
                           num(w[["Vent fort, tempête"]])))
    expect_has(t4, sprintf("mais sur %s usagers", num(sum(s$atm %in% c("Brouillard, fumée", "Vent fort, tempête")), 0)))
    ages <- tapply(s$age, s$grave, mean)
    expect_has(t4, sprintf("(%s ans contre %s)", num(ages[["oui"]]), num(ages[["non"]])))

    session$setInputs(q3_var = "age")
    x <- s$age[s$grave == "oui"]
    expect_has(plain(output$q3_detail), num(sqrt(mean((x - mean(x))^2))))
    expect_has(plain(output$q3_replay), "Variable X age (âge), Variable Y grave (usager grave)")
    session$setInputs(q3_var = "vma")
    expect_has(plain(output$q3_replay), "« Variables à retyper » an (année) et vma (vitesse maximale autorisée), « Nouveau type » Qualitative (quali)")
    expect_has(plain(output$q3_replay), "Variable X vma (vitesse maximale autorisée), Variable Y grave (usager grave)")
    session$setInputs(q3_var = "catr")
    expect_false(grepl("retyper", plain(output$q3_replay)))
    detail <- plain(output$q3_detail)
    expect_has(detail, sprintf("Route départementale %s %s", num(sum(s$catr == "Route départementale"), 0),
                               num(sum(s$catr == "Route départementale" & s$grave == "oui"), 0)))
  })
})

test_that("the vma sentence names the cells with a small expected count only when there are some", {
  s <- fake_sample()
  s$vma[1:3] <- 15  # a rare speed limit
  tab <- table(s$vma, s$grave)
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  shiny::testServer(mod_case_study_server, args = list(baac_sample = s, final_results = NULL), {
    expect_has(plain(output$q3_text), sprintf("en partie gonflé par des valeurs rares (%d cellules sur %d ont",
                                              sum(expected < 5), length(expected)))
  })
  shiny::testServer(mod_case_study_server, args = list(baac_sample = fake_sample(), final_results = NULL), {
    expect_false(grepl("gonflé", plain(output$q3_text), fixed = TRUE))
  })
})

test_that("every variable quoted has its meaning, and the text says qualitative and quantitative", {
  final <- fake_final()
  codes <- c(CASE_STUDY_TARGET, "grav", "an", final$specs$ablation$vars, Q3_VARS)
  expect_true(all(codes %in% names(VARIABLE_LABELS)))
  expect_true(all(codes %in% names(VARIABLE_DESCRIPTIONS)))
  expect_equal(variable_label("vma"), "vitesse maximale autorisée (vma)")
  expect_error(variable_label("inconnue"))
  g <- variable_glossary(final$specs$main$vars, final$specs$ablation$vars)
  expect_equal(g$Rôle[g$Code == "grave"], "cible")
  expect_equal(g$Rôle[g$Code == "col"], "ablation seulement")
  expect_equal(g$Rôle[g$Code == "catu"], "modèle principal")
  expect_equal(g$Type[g$Code == "age"], "quantitative")
  expect_equal(g$Type[g$Code == "catu"], "qualitative")
  expect_match(g$Type[g$Code == "vma"], "^quantitative")
  expect_false(anyDuplicated(g$Code) > 0)
  expect_equal(pretty_term(c("secu_ceinturenon", "vma", "catrAutoroute", "x1"), c("secu_ceinture", "vma", "catr")),
               c("ceinture (secu_ceinture) : non", "vitesse maximale autorisée (vma)",
                 "type de route (catr) : Autoroute", "x1"))
  # catv_groupe must not be read as catv followed by a level.
  expect_equal(pretty_term("catv_groupeVélo", c("catv", "catv_groupe", "catu")), "catégorie de véhicule (catv_groupe) : Vélo")
  expect_equal(pretty_term("catvVL", c("catv", "catv_groupe")), "catégorie de véhicule détaillée (catv) : VL")
  expect_equal(g$Type[g$Code == "an"], "quantitative (qualitative pour Q1)")
  expect_equal(g$Type[g$Code == "vma"], "quantitative (qualitative pour Q3)")
  expect_equal(g$Rôle[g$Code == "catv"], "écartée des modèles")
  expect_equal(g$Type[g$Code == "nbv"], "quantitative")
  expect_equal(g$Rôle[g$Code == "Num_Acc"], "identifiant")
  expect_equal(g$Type[g$Code == "dep"], "identifiant")
  expect_setequal(g$Code, names(VARIABLE_DESCRIPTIONS))
  without <- variable_glossary(character(0), character(0))
  expect_setequal(without$Code, names(VARIABLE_DESCRIPTIONS))
  expect_equal(without$Rôle[without$Code == "catu"], "variable des modèles")
  expect_equal(without$Rôle[without$Code == "grave"], "cible")
  expect_identical(names(VARIABLE_LABELS), names(VARIABLE_DESCRIPTIONS))
  # Fixed texts whose meaning matters.
  d <- VARIABLE_DESCRIPTIONS
  expect_match(d[["grave"]], "tué ou blessé hospitalisé, « non » s'il est indemne ou blessé léger", fixed = TRUE)
  expect_match(d[["catu"]], "conducteur, passager ou piéton", fixed = TRUE)
  expect_match(d[["catv_groupe"]], "en 13 classes", fixed = TRUE)
  expect_match(d[["catv_groupe"]], "un piéton est rattaché au véhicule qui l'a heurté", fixed = TRUE)
  expect_match(d[["obs"]], "^obstacle fixe")
  expect_match(d[["obsm"]], "^obstacle mobile")
  expect_match(d[["vma"]], "en km/h", fixed = TRUE)
  imp <- labelled_importance(final$specs$main$importance)
  expect_equal(imp$ranger$variable[1], "ceinture (secu_ceinture)")
  expect_equal(imp$glm$importance, final$specs$main$importance$glm$importance)
  ds <- case_study_dataset(fake_sample())
  pp <- profile_plot(grave_profile(ds, "vma"))
  expect_equal(pp$labels$x, "vitesse maximale autorisée (vma)")
  expect_equal(pp$labels$fill, "usager grave (grave)")
  expect_equal(severity_plot(severity_table(ds))$labels$x, "gravité en quatre niveaux (grav)")
  ap <- age_plot(ds)
  expect_equal(c(ap$labels$x, ap$labels$y), c("usager grave (grave)", "âge (age)"))

  shiny::testServer(mod_case_study_server, args = list(baac_sample = fake_sample(), final_results = final), {
    texts <- character(0)
    for (v in Q3_VARS) {
      session$setInputs(q3_var = v)
      texts <- c(texts, plain(output$q3_replay))
    }
    for (o in c("approach", "severity_text", "year_text", "catu_text", "catv_text", "q3_text", "test_text",
                "balance_text", "ablation_text", "importance_text", "logistic_text", "coherence_text",
                "model_replay", "limits", "conclusions", "glossary", "q3_table", "logistic_table")) {
      texts <- c(texts, plain(output[[o]]))
    }
    texts <- c(texts, plain(htmltools::renderTags(mod_case_study_ui("cs"))$html))
    # "Qualitative (quali)" is the label of tab 1; anything else says the word in full.
    stripped <- gsub("Qualitative (quali)", "", paste(texts, collapse = " "), fixed = TRUE)
    expect_false(grepl("\\b(quali|quanti)\\b", stripped, perl = TRUE))
    expect_has(plain(output$logistic_table), "ceinture (secu_ceinture) : non")
    expect_has(plain(output$glossary), "vitesse maximale autorisée sur le lieu et au moment de l'accident")
    expect_has(plain(output$glossary), "obsm obstacle mobile heurté (piéton, véhicule, animal...) qualitative ablation seulement")
    expect_has(plain(output$glossary), "catu conducteur, passager ou piéton qualitative modèle principal")
    ui <- plain(htmltools::renderTags(mod_case_study_ui("cs"))$html)
    expect_has(ui, "l'AUC (aire sous cette courbe) est la probabilité qu'un usager grave tiré au hasard reçoive une probabilité plus haute qu'un usager non grave")
    expect_has(ui, "le Recall la part des usagers graves retrouvés, et le F1 leur moyenne harmonique")
    expect_has(plain(output$approach), "mtry, le nombre de variables tirées au hasard à chaque coupure")
    expect_has(plain(output$approach), "Observatoire national interministériel de la sécurité routière (ONISR)")
  })
})

test_that("the static replay steps retype an and vma where the figure needs it", {
  html <- plain(htmltools::renderTags(mod_case_study_ui("cs"))$html)
  # Q1 by year and the Q3 table need the retype; the other static boxes do not.
  expect_equal(lengths(regmatches(html, gregexpr("Variables à retyper", html, fixed = TRUE))), 2)
  expect_has(html, "Variable X an (année), Variable Y grave (usager grave)")
  expect_has(html, "Variable X catu (catégorie d'usager), Variable Y grave (usager grave)")
})

test_that("the model sentences read the right variants and intervals", {
  final <- fake_final()
  shiny::testServer(mod_case_study_server, args = list(baac_sample = fake_sample(), final_results = final), {
    t1 <- plain(output$test_text)
    expect_has(t1, "La forêt aléatoire a la meilleure AUC sur 2024 (0,845), devant la régression logistique (0,810) et l'arbre de décision (0,780)")
    expect_has(t1, "la forêt aléatoire retrouve 66,0 % des usagers graves (Recall), et 46,0 % des usagers signalés")
    expect_has(t1, "(F1 = 0,333)")
    t2 <- plain(output$balance_text)
    expect_has(t2, "écarts de 0,0100 [0,0070 ; 0,0130] et 0,0050 [0,0020 ; 0,0080]")
    expect_has(t2, "(0,600 contre 0,200 pour la logistique)")
    expect_has(t2, "L'arbre gagne davantage (0,0300 [0,0270 ; 0,0330])")
    expect_has(plain(output$ablation_text), "Ajouter col, choc, obs et obsm augmente l'AUC de 0,000 à 0,030")
    expect_has(plain(output$test_table), "0,845 [0,842 ; 0,848]")
    expect_false(grepl("0,865", plain(output$test_table), fixed = TRUE))
    t3 <- plain(output$importance_text)
    expect_has(t3, "calculée sur 200 usagers")
    expect_has(t3, "Suivent, pour la forêt : manv, situ, agg, catv_groupe, age.")
    t4 <- plain(output$logistic_text)
    expect_has(t4, "coefficient de la logistique pondérée")
    expect_has(t4, "ceinture non (contre oui) 4,70 [4,50 ; 4,90], piéton (contre conducteur) 2,30 [2,20 ; 2,40], hors agglomération 1,90 [1,80 ; 2,00], autoroute (contre route départementale) 0,32 [0,31 ; 0,34]")
    t5 <- plain(output$coherence_text)
    expect_has(t5, "(rangs 1, 5, 4, 7 et 8 sur 9)")
    expect_has(t5, "qu'au rang 9")
    expect_has(t5, "multipliées par 4,7")
    t6 <- plain(output$model_replay)
    removed <- regmatches(output$model_replay$html, gregexpr("(?<=<code>)[^<]*", output$model_replay$html, perl = TRUE))[[1]]
    expect_has(t6, "an (année), grav (gravité en quatre niveaux), lum (luminosité)")
    expect_has(t6, "Num_Acc (identifiant de l'accident)")
    expect_true(all(c("an", "grav", "col", "choc", "obs", "obsm", "lum") %in% removed))
    expect_false(any(c("catu", "grave", "Num_Acc", "agg", "age") %in% removed))
    expect_has(t6, "« Modalités rares : seuil » 0.5 (avec un point)")
    expect_has(t6, "« Rééquilibrage de l'entraînement » Pondération des classes")
    expect_has(t6, "l'ordre des modèles peut changer")
    expect_has(t6, "grav en particulier : c'est la variable dont la cible est tirée")
    expect_has(t6, "que l'alerte de fuite de l'onglet Modélisation signale")
    t7 <- plain(output$conclusions)
    expect_has(t7, "la forêt aléatoire (AUC 0,845) fait un peu mieux que la régression logistique (0,810), elle-même devant l'arbre (0,780) ; F1 de 0,480 à 0,540, contre 0,333")
    t8 <- plain(output$limits)
    expect_has(t8, "le meilleur cp de l'arbre (0.001) est à l'intérieur de la grille")
    expect_has(t8, "(AUC de la grille entre 0,840 et 0,845 sur 2023)")
    expect_has(t8, "d'au plus 1,5 % de 2019 à 2022 à 3,5 % en 2023")
    t9 <- plain(output$approach)
    expect_has(t9, "choix des réglages et du seuil sur 2023 (300 usagers), une seule évaluation sur 2024 (500 usagers, 250 accidents)")
    expect_has(t9, "Ablation : col, choc, obs et obsm")
    expect_has(t9, "à chaque coupure, dans {3, 5} ; min.node.size, la taille minimale d'un nœud pour qu'il soit encore coupé, dans {50}) et arbre de décision (cp, l'amélioration relative minimale de l'ajustement pour couper un nœud, dans {3e-04, 0.001})")
    expect_has(t9, "modalités de moins de 0,5 % regroupées")
    expect_has(t9, "Échantillon de l'app : 7 % des accidents")
  })
  # A logistic regression chosen without rebalancing is named so, and two
  # different rebalancings give a replay step per variant.
  final$specs$main$test$chosen <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  shiny::testServer(mod_case_study_server, args = list(baac_sample = NULL, final_results = final), {
    expect_has(plain(output$logistic_text), "coefficient de la logistique sans rééquilibrage")
    expect_has(plain(output$model_replay), "celui de la variante choisie")
    expect_has(plain(output$limits), "le meilleur cp de l'arbre (0.001)")
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
  # "Brouillard et vent fort donnent des parts plus hautes".
  atm <- pr("atm")
  expect_gt(min(share_of(atm, "Brouillard, fumée"), share_of(atm, "Vent fort, tempête")), share_of(atm, "Normale"))
  # "Le véhicule est l'une des conditions les plus liées": third Cramér of Q2 and Q3.
  cramer <- vapply(c("catu", "catv_groupe", setdiff(Q3_VARS, "age")), function(v) pr(v)$stats$cramer, numeric(1))
  expect_lte(match("catv_groupe", names(sort(cramer, decreasing = TRUE))), 3)
  # Conclusion: users without a body shell against the occupants of light vehicles.
  occupants <- ds$data$catv_groupe == "VL" & ds$data$catu != "Piéton"
  base <- 100 * mean(ds$data$grave[occupants] == "oui")
  expect_gt(min(share_of(catu, "Piéton"), share_of(catv, "Vélo"), share_of(catv, "2RM/3RM <= 50 cm3")), 1.5 * base)
  # "La part de graves d'une année varie d'environ un point par le seul tirage".
  sample_range <- diff(range(pr("an")$share))
  expect_gt(sample_range, diff(range(FULL_TABLE_YEARS$grave_pct)))
  expect_lt(max(abs(pr("an")$share - FULL_TABLE_YEARS$grave_pct)), 1.5)
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
  years <- full[full$an <= 2023, ]
  vma_na <- round(100 * tapply(is.na(years$vma), years$an, mean), 2)
  expect_equal(as.vector(vma_na), unname(VMA_NA_PCT))
  skip_if_not(file.exists(sample_path))
  sample <- readRDS(sample_path)
  ratio <- tapply(sample$Num_Acc, sample$an, function(x) length(unique(x))) /
    tapply(full$Num_Acc, full$an, function(x) length(unique(x)))
  expect_true(all(abs(ratio - SAMPLE_FRACTION) < 0.001))
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
  # "Tous les modèles dépassent nettement « tout positif »".
  f1 <- vapply(ev, function(e) e$metrics[["f1"]], numeric(1))
  expect_gt(min(f1), 1.5 * final$naive_test$all_positive[["f1"]])
  # Coherence: equipment, vehicle, agglomeration, speed limit and road among the
  # ten most important variables of the forest.
  expect_true(all(match(c("secu_ceinture", "catv_groupe", "agg", "vma", "catr"), forest$variable) <= 10))
  # "Probabilités des modèles pondérés décalées vers le haut".
  t <- spec$test
  expect_true(all(t$threshold[t$balance == "weights"] > t$threshold[t$balance == "none"] + 0.2))
  expect_equal(crash_vars(final), c("col", "choc", "obs", "obsm"))
  # Every variable of the models has its meaning, and the types of the table of
  # the variables are those the app detects.
  expect_true(all(final$specs$ablation$vars %in% names(VARIABLE_DESCRIPTIONS)))
  expect_true(all(final$specs$ablation$vars %in% names(VARIABLE_LABELS)))
  expect_true(all(pretty_term(spec$logistic$term, spec$vars) != spec$logistic$term))
  skip_if_not(file.exists(sample_path))
  sample <- readRDS(sample_path)
  expect_setequal(names(sample), names(VARIABLE_DESCRIPTIONS))
  types <- detect_types(sample)
  expect_setequal(names(types)[types == "quanti"], c(QUANTITATIVE_VARS, EXCLUDED_QUANTITATIVE, "an"))
  expect_setequal(names(types)[types == "id"], c(IDENTIFIER_VARS, "dep"))
})
