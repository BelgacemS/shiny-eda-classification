# Expected values recomputed independently (by hand, or with base R corrected
# by n / (n - 1)); the slides are only a cross-check.

course_file <- function(name) test_path("fixtures", "course", name)

test_that("secteursActivite.csv: r(EF, CA) = 0.661, slope 0.509, intercept 20884", {
  path <- course_file("secteursActivite.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  s <- read_table_file(path)
  res <- quanti_quanti(s$EF, s$CA)
  expect_equal(round(res$r, 3), 0.661)
  expect_equal(round(res$slope, 3), 0.509)
  expect_equal(round(res$intercept), 20884)
  # Independent check: base R correlation (the n - 1 factors cancel) and lm().
  expect_equal(res$r, stats::cor(s$EF, s$CA))
  expect_equal(unname(c(res$intercept, res$slope)), unname(stats::coef(stats::lm(CA ~ EF, s))))
  expect_equal(res$covariance, stats::cov(s$EF, s$CA) * (nrow(s) - 1) / nrow(s))
})

test_that("chiens.csv in long format: c = 0.598 in 1/n and s_E^2 + s_R^2 = s_Y^2", {
  path <- course_file("chiens.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  d <- read_table_file(path)
  y <- unlist(d, use.names = FALSE)
  g <- rep(names(d), each = nrow(d))
  res <- correlation_ratio(y, g)
  expect_equal(round(res$c, 3), 0.598)
  expect_equal(res$s_e2 + res$s_r2, res$s_y2)
  # The slide gives 0.59: the course script computes the within variances with
  # var(), in n - 1, which gives 0.588.
  n_k <- res$groups$n
  s_r2_course <- sum(n_k * tapply(y, g, stats::var)[res$groups$level]) / length(y)
  expect_equal(round(sqrt(res$s_e2 / (res$s_e2 + s_r2_course)), 3), 0.588)
})

test_that("correlation ratio on a toy set computed by hand", {
  # Groups a: 1, 3 (mean 2) and b: 5, 7 (mean 6); overall mean 4.
  # s_E^2 = (2 * 4 + 2 * 4) / 4 = 4; s_R^2 = (1 + 1 + 1 + 1) / 4 = 1; s_Y^2 = 5.
  res <- correlation_ratio(c(1, 3, 5, 7, NA), c("a", "a", "b", "b", "b"))
  expect_equal(c(res$s_e2, res$s_r2, res$s_y2), c(4, 1, 5))
  expect_equal(res$c, sqrt(4 / 5))
  expect_equal(res$n, 4L)
  expect_true(is.na(correlation_ratio(c(2, 2, 2), c("a", "b", "b"))$c))
})

test_that("Sex x Level of students.csv: chi2 12.82, Phi2 0.267, T 0.346, C 0.517", {
  path <- course_file("students.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  st <- read_table_file(path)
  tab <- contingency_table(st$Sex, st$Level)
  res <- chi2_indices(tab)
  # 7 of the 12 cells have an expected count below 5, hence the warning of chisq.test.
  expect_equal(res$chi2, unname(suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic))
  expect_equal(round(res$chi2, 2), 12.82)
  expect_equal(round(res$phi2, 3), 0.267)
  expect_equal(round(res$tschuprow, 3), 0.346)
  expect_equal(round(res$cramer, 3), 0.517)
  expect_equal(res$n_expected_below_5, 7L)
})

test_that("chi2 indices on a 2 x 2 table computed by hand", {
  # Table 10 0 / 0 10: expected 5 everywhere, chi2 = 4 * 25 / 5 = 20, n = 20,
  # Phi2 = 1, T = C = 1.
  tab <- matrix(c(10, 0, 0, 10), 2)
  res <- chi2_indices(tab)
  expect_equal(c(res$chi2, res$phi2, res$tschuprow, res$cramer), c(20, 1, 1, 1))
  expect_true(is.na(chi2_indices(matrix(c(3, 4), 1))$cramer))
  # Rows 1 1 and 3 1: row profiles 0.5 0.5 and 0.75 0.25.
  prof <- row_profiles(as.table(matrix(c(1, 3, 1, 1), 2)))
  expect_equal(as.vector(prof[1, ]), c(0.5, 0.5))
  expect_equal(as.vector(prof[2, ]), c(0.75, 0.25))
})

test_that("constant or empty pairs give NA or NULL without error", {
  res <- quanti_quanti(c(1, 1, 1), c(1, 2, 3))
  expect_true(is.na(res$r))
  expect_true(is.na(res$slope))
  expect_null(quanti_quanti(c(NA, 1), c(2, NA)))
})

test_that("association_table uses the index of each pair of types", {
  set.seed(5)
  n <- 200
  data <- data.frame(y = factor(sample(c("non", "oui"), n, TRUE)), x = rnorm(n),
                     g = factor(sample(c("a", "b", "c"), n, TRUE)), id = seq_len(n))
  data$copy <- data$y
  ds <- make_dataset(data, c(y = "quali", x = "quanti", g = "quali", id = "id", copy = "quali"))
  t <- association_table(ds, "y")
  expect_setequal(t$variable, c("x", "g", "copy"))
  expect_equal(t$index[t$variable == "x"], "Rapport de corrélation c")
  expect_equal(t$index[t$variable == "g"], "C de Cramér")
  expect_equal(t$value[t$variable == "copy"], 1)
  t <- association_table(ds, "x")
  expect_equal(t$index[t$variable == "y"], "Rapport de corrélation c")
})

test_that("thin_indices keeps at most max_points regular indices", {
  expect_equal(thin_indices(5, 10), 1:5)
  idx <- thin_indices(100000, 5000)
  expect_lte(length(idx), 5000)
  expect_equal(range(idx), c(1, 100000))
})

test_that("the bivariate module renders every pair type and the association table", {
  set.seed(6)
  n <- 60
  ds <- make_dataset(
    data.frame(a = rnorm(n), b = rnorm(n), g = sample(c("u", "v"), n, TRUE),
               h = sample(c("p", "q", "r"), n, TRUE)),
    c(a = "quanti", b = "quanti", g = "quali", h = "quali")
  )
  shiny::testServer(mod_explore_bi_server, args = list(dataset_clean = shiny::reactive(ds)), {
    session$setInputs(x = "a", y = "b", target = "g", profile = "fill")
    expect_equal(output$pair, "qq")
    expect_match(output$pair_info, "60 cas complets")
    for (o in c("scatter", "backback", "scatter_hist")) expect_true(nchar(output[[o]]$src) > 0)
    session$setInputs(x = "b", y = "g")
    expect_equal(output$pair, "nc")
    expect_equal(pair()$y_name, "b")
    expect_true(nchar(output$boxes$src) > 0)
    session$setInputs(x = "g", y = "h")
    expect_equal(output$pair, "cc")
    expect_match(output$contingency, "Total")
    expect_true(nchar(output$profiles$src) > 0)
    expect_equal(nrow(associations()), 3L)
  })
})
