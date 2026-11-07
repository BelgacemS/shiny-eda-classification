# Property tests: random data sets with the traps met so far (missing values,
# constant decimal columns, a single row, rare and unseen levels, values on
# class boundaries), checked against invariants and against base R references,
# not against fixed expected values.

# 150 draws per property by default; a deeper local run: PROPERTY_DRAWS=2000.
N_DRAWS <- as.integer(Sys.getenv("PROPERTY_DRAWS", "150"))

# A random quanti vector of length n, of a random kind.
random_quanti <- function(n) {
  kind <- sample(c("normal", "decimal", "integer", "constant_dec", "constant_int", "with_na",
                   "all_na", "skewed"), 1)
  x <- switch(kind,
    normal = rnorm(n, sample(c(0, 100, 1e5), 1), sample(c(0.01, 1, 1000), 1)),
    decimal = round(runif(n, -5, 5), sample(1:2, 1)),
    integer = sample(0:9, n, replace = TRUE),
    constant_dec = rep(sample(c(0.1, 0.2, 1 / 3, 0.7, 2.3, -1.1), 1), n),
    constant_int = rep(4, n),
    with_na = replace(rnorm(n), sample.int(n, ceiling(n / 3)), NA),
    all_na = rep(NA_real_, n),
    skewed = rexp(n)^3
  )
  as.numeric(x)
}

# A random quali factor of length n, with rare levels, a single level or NA.
random_quali <- function(n) {
  kind <- sample(c("balanced", "rare", "single", "with_na", "many"), 1)
  x <- switch(kind,
    balanced = sample(c("a", "b", "c"), n, replace = TRUE),
    rare = sample(c("a", "b", "rare1", "rare2"), n, replace = TRUE, prob = c(0.6, 0.36, 0.02, 0.02)),
    single = rep("seul", n),
    with_na = replace(sample(c("u", "v"), n, replace = TRUE), sample.int(n, ceiling(n / 3)), NA),
    many = sample(sprintf("m%02d", 1:25), n, replace = TRUE)
  )
  factor(x)
}

random_dataset <- function(n) {
  data <- data.frame(q1 = random_quanti(n), q2 = random_quanti(n), q3 = random_quanti(n),
                     g1 = random_quali(n), g2 = random_quali(n))
  make_dataset(data, c(q1 = "quanti", q2 = "quanti", q3 = "quanti", g1 = "quali", g2 = "quali"))
}

random_n <- function() sample(c(1, 2, 3, 5, 12, 40, 150), 1)

tol <- 1e-8

# Oracle of constancy written independently of is_constant().
single_value <- function(x) length(unique(x)) == 1

# Class counts written from the definition: ]b_{i-1}, b_i], the first class
# closed, with the tolerance that hist() documents (1e-7 of the median class
# width), so that a decimal read on a boundary stays in the lower class.
reference_class_counts <- function(x, breaks) {
  fuzz <- 1e-7 * stats::median(diff(breaks))
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  vapply(seq_along(lower), function(i) {
    above <- if (i == 1) x >= lower[i] - fuzz else x > lower[i] + fuzz
    as.integer(sum(above & x <= upper[i] + fuzz))
  }, integer(1))
}

test_that("univariate statistics keep their invariants on random data", {
  withr::local_seed(101)
  for (i in seq_len(N_DRAWS)) {
    x <- random_quanti(random_n())
    obs <- x[!is.na(x)]
    ind <- uni_indicators(x)
    if (length(obs) == 0) {
      expect_null(ind)
      next
    }
    expect_gte(ind$variance, 0)
    expect_equal(ind$sd, sqrt(ind$variance))
    expect_true(ind$min <= ind$q1 + tol && ind$q1 <= ind$median + tol &&
                  ind$median <= ind$q3 + tol && ind$q3 <= ind$max + tol)
    expect_true(ind$mean >= ind$min - tol * max(1, abs(ind$min)) &&
                  ind$mean <= ind$max + tol * max(1, abs(ind$max)))
    if (length(obs) > 1) {
      expect_equal(ind$variance, stats::var(obs) * (length(obs) - 1) / length(obs))
    }
    values <- freq_table_values(x)
    expect_equal(sum(values$n), length(obs))
    expect_equal(tail(values$F, 1), 1)
    for (rule in CLASS_RULES) {
      breaks <- class_breaks(x, rule)
      classes <- freq_table_classes(x, breaks)
      expect_equal(sum(classes$n), length(obs))
      expect_equal(classes$n, reference_class_counts(obs, breaks))
      expect_equal(sum(classes$density * classes$width), 1)
      expect_lte(nrow(classes), 2 * MAX_CLASSES)
    }
  }
})

test_that("bivariate indices stay in their range and match base R", {
  withr::local_seed(202)
  for (i in seq_len(N_DRAWS)) {
    n <- random_n()
    x <- random_quanti(n)
    y <- random_quanti(n)
    g <- random_quali(n)
    h <- random_quali(n)

    qq <- quanti_quanti(x, y)
    keep <- !is.na(x) & !is.na(y)
    if (!any(keep)) {
      expect_null(qq)
    } else if (single_value(x[keep]) || single_value(y[keep])) {
      expect_true(is.na(qq$r))
    } else {
      expect_true(abs(qq$r) <= 1 + tol)
      expect_equal(qq$r, stats::cor(x[keep], y[keep]))
      # lm() on centred x: on raw x such as 1e5 +/- 0.01, lm() declares x collinear
      # with the intercept (tolerance 1e-7) and returns NA, while the slope exists.
      xk <- x[keep]
      fit <- unname(stats::coef(stats::lm(y[keep] ~ I(xk - mean(xk)))))
      expect_equal(qq$slope, fit[2], tolerance = 1e-6)
      expect_equal(qq$intercept, fit[1] - fit[2] * mean(xk), tolerance = 1e-6)
    }

    cr <- correlation_ratio(x, g)
    keep <- !is.na(x) & !is.na(g)
    if (!any(keep)) {
      expect_null(cr)
    } else {
      expect_equal(cr$s_e2 + cr$s_r2, cr$s_y2, tolerance = 1e-8)
      if (single_value(x[keep])) {
        expect_true(is.na(cr$c))
      } else {
        expect_true(cr$c >= -tol && cr$c <= 1 + tol)
      }
    }

    tab <- contingency_table(g, h)
    ci <- chi2_indices(tab)
    if (sum(tab) == 0) {
      expect_null(ci)
    } else {
      expect_gte(ci$chi2, -tol)
      if (nrow(tab) > 1 && ncol(tab) > 1) {
        expect_equal(ci$chi2, unname(suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic))
        expect_true(ci$cramer >= -tol && ci$cramer <= 1 + tol)
        # T <= C, since sqrt((r - 1)(c - 1)) >= min(r, c) - 1.
        expect_lte(ci$tschuprow, ci$cramer + tol)
      } else {
        expect_true(is.na(ci$cramer))
      }
    }
  }
})

random_recipe <- function() {
  list(
    outliers = list(method = sample(c("tukey", "zscore"), 1), z = sample(c(0.5, 1, 3), 1),
                    action = sample(c("none", "flag", "cap", "remove"), 1)),
    impute = list(quanti = sample(c("none", "median", "mean"), 1),
                  quali = sample(c("none", "mode", "missing_level"), 1)),
    rare = list(threshold = sample(c(0, 0.05, 0.3), 1)),
    scale = list(method = sample(c("none", "standard", "minmax"), 1)),
    dummy = list(method = sample(c("none", "one_hot", "k_minus_1"), 1),
                 max_cols = sample(c(2, 50), 1))
  )
}

test_that("the recipe keeps its contract on random data and settings", {
  withr::local_seed(303)
  for (i in seq_len(N_DRAWS)) {
    train <- random_dataset(random_n())
    test <- random_dataset(random_n())
    # Unseen levels in the test set.
    levels(test$data$g1) <- paste0(levels(test$data$g1), sample(c("", "_new"), 1))
    recipe <- random_recipe()
    fitted <- fit_recipe(recipe, train$data, train$var_types)
    out_train <- fitted$train
    out_test <- apply_recipe(fitted, test$data)

    # The test set is never cut, and both outputs have the same columns.
    expect_equal(nrow(out_test), nrow(test$data))
    expect_identical(names(out_test), names(out_train))
    expect_true(all(fitted$kept_rows %in% seq_len(nrow(train$data))))
    expect_equal(nrow(out_train), length(fitted$kept_rows))
    if (recipe$outliers$action != "remove") {
      expect_identical(apply_recipe(fitted, train$data), out_train)
    }
    # No NaN nor infinite value is created.
    for (out in list(out_train, out_test)) {
      numeric_cols <- vapply(out, is.numeric, logical(1))
      expect_false(any(vapply(out[numeric_cols], function(v) any(is.nan(v) | is.infinite(v)), logical(1))))
    }
    # Every test level is a training level: no factor of the output has an unseen level.
    for (v in names(out_test)[vapply(out_test, is.factor, logical(1))]) {
      expect_true(all(levels(out_test[[v]]) %in% levels(out_train[[v]])))
      expect_identical(levels(out_test[[v]]), levels(out_train[[v]]))
    }
    # Nothing is learned on new data: applying the recipe row by row gives the
    # same result as applying it to the whole test set.
    rows <- seq_len(min(nrow(test$data), 6))
    one_by_one <- do.call(rbind, lapply(rows, function(i) apply_recipe(fitted, test$data[i, , drop = FALSE])))
    expect_equal(as.list(one_by_one), as.list(out_test[rows, , drop = FALSE]))
    # Without imputation, no observed value becomes NA (an unseen level goes to a
    # training level, scaling and capping keep observed values).
    if (recipe$impute$quanti == "none" && recipe$impute$quali == "none") {
      dummies <- fitted$params$dummy$params
      for (v in names(test$data)) {
        observed_in <- !is.na(test$data[[v]])
        cols <- if (v %in% names(dummies)) dummies[[v]]$names else intersect(v, names(out_test))
        has_train_levels <- !is.factor(out_train[[v]]) || length(levels(out_train[[v]])) > 0
        if (length(cols) == 0 || !has_train_levels) next
        missing_out <- Reduce(`|`, lapply(out_test[cols], is.na))
        expect_false(any(missing_out & observed_in))
      }
    }
    # Standard scaling: mean 0 and sd (1/n) 1 on train for every complete scaled column.
    if (recipe$scale$method == "standard") {
      for (v in names(fitted$params$scale$params)) {
        col <- out_train[[v]]
        if (length(col) > 0 && !anyNA(col)) {
          expect_equal(mean_1n(col), 0, tolerance = 1e-8)
          expect_equal(sd_1n(col), 1, tolerance = 1e-8)
        }
      }
    }
  }
})

test_that("rebalancing keeps valid and test, and reaches its target, on random targets", {
  withr::local_seed(404)
  for (i in seq_len(N_DRAWS)) {
    n <- sample(c(4, 10, 37, 200), 1)
    p <- runif(1, 0.05, 0.5)
    y <- c(1L, 0L, stats::rbinom(n - 2, 1, p))  # both classes present
    part <- function(y) list(x = data.frame(a = seq_along(y)), y = y)
    split <- list(train = part(y), valid = part(rev(y)), test = part(y[1:2]))
    ratio <- sample(c(0.25, 0.5, 1), 1)
    for (method in c("weights", "under", "over")) {
      out <- balance_train(split, method, ratio = ratio, seed = i)
      expect_identical(out$valid, split$valid)
      expect_identical(out$test, split$test)
      expect_equal(nrow(out$train$x), length(out$train$y))
      if (method == "weights") {
        w <- out$train$weights
        expect_equal(sum(w[out$train$y == 1]), sum(w[out$train$y == 0]))
      } else {
        # Exact counts from the definition; on a tie, the first class in sorted
        # order is the minority.
        before <- table(as.character(y))
        minority <- names(before)[order(as.numeric(before))[1]]
        majority <- setdiff(names(before), minority)
        n_min <- before[[minority]]
        n_maj <- before[[majority]]
        after <- table(as.character(out$train$y))
        if (method == "under") {
          expect_equal(after[[minority]], n_min)
          expect_equal(after[[majority]], min(n_maj, max(1, round(n_min / ratio))))
        } else {
          expect_equal(after[[majority]], n_maj)
          expect_equal(after[[minority]], max(n_min, round(ratio * n_maj)))
        }
      }
    }
  }
})
