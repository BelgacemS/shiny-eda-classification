# Bivariate statistics and association indices of the course, computed by hand
# on the complete cases of the pair, with the 1/n convention.

# Constancy is read on the data: the variance of a constant decimal column
# (rep(0.1, 50)) is about 1e-33 after rounding, not 0, and would pass a "> 0" test.
is_constant <- function(x) {
  length(x) == 0 || max(x) == min(x)
}

# Covariance, linear correlation and least squares line y = a x + b:
# a = s_XY / s_X^2, b = mean(y) - a mean(x). r and the line are NA when x or y
# is constant.
quanti_quanti <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) == 0) {
    return(NULL)
  }
  s_xy <- cov_1n(x, y)
  s_x <- sd_1n(x)
  s_y <- sd_1n(y)
  a <- if (!is_constant(x)) s_xy / s_x^2 else NA_real_
  list(
    n = length(x),
    covariance = s_xy,
    r = if (!is_constant(x) && !is_constant(y)) s_xy / (s_x * s_y) else NA_real_,
    slope = a,
    intercept = mean_1n(y) - a * mean_1n(x)
  )
}

# Correlation ratio of a quanti y by the groups g: c = s_E / s_Y, with
# s_E^2 = (1/n) sum n_k (mean_k - mean)^2 (between groups) and
# s_R^2 = (1/n) sum_k sum_i (y_ki - mean_k)^2 (within groups), s_E^2 + s_R^2 = s_Y^2.
# Groups without observation are dropped; c is NA when y is constant.
correlation_ratio <- function(y, g) {
  keep <- !is.na(y) & !is.na(g)
  y <- y[keep]
  g <- droplevels(as.factor(g[keep]))
  n <- length(y)
  if (n == 0) {
    return(NULL)
  }
  overall <- mean_1n(y)
  n_k <- as.vector(table(g))
  mean_k <- as.vector(tapply(y, g, mean_1n))
  var_k <- as.vector(tapply(y, g, var_1n))
  s_e2 <- sum(n_k * (mean_k - overall)^2) / n
  s_r2 <- sum(n_k * var_k) / n
  s_y2 <- var_1n(y)
  list(
    n = n,
    groups = data.frame(level = levels(g), n = n_k, mean = mean_k, sd = sqrt(var_k)),
    s_e2 = s_e2,
    s_r2 = s_r2,
    s_y2 = s_y2,
    c = if (!is_constant(y)) sqrt(s_e2 / s_y2) else NA_real_
  )
}

# Contingency table of the complete cases; levels without observation are dropped.
contingency_table <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  table(droplevels(as.factor(x[keep])), droplevels(as.factor(y[keep])))
}

# Chi-squared with the expected counts n_i. n_.j / n, Phi^2 = chi^2 / n,
# Tschuprow T = sqrt(Phi^2 / sqrt((r - 1)(c - 1))), Cramer
# C = sqrt(Phi^2 / (min(r, c) - 1)). T and C are NA with a single row or column.
chi2_indices <- function(tab) {
  n <- sum(tab)
  if (n == 0) {
    return(NULL)
  }
  expected <- outer(rowSums(tab), colSums(tab)) / n
  chi2 <- sum((tab - expected)^2 / expected)
  phi2 <- chi2 / n
  r <- nrow(tab)
  k <- ncol(tab)
  list(
    n = n,
    chi2 = chi2,
    phi2 = phi2,
    tschuprow = if (r > 1 && k > 1) sqrt(phi2 / sqrt((r - 1) * (k - 1))) else NA_real_,
    cramer = if (min(r, k) > 1) sqrt(phi2 / (min(r, k) - 1)) else NA_real_,
    n_expected_below_5 = sum(expected < 5),
    n_cells = r * k
  )
}

# Row profiles: each row of the table divided by its total.
row_profiles <- function(tab) {
  sweep(tab, 1, pmax(rowSums(tab), 1), "/")
}

# Index of association between a variable and the target, by pair of types:
# r for quanti / quanti, c for quanti / quali (quanti as y), Cramer C for
# quali / quali. Returns list(index, value, n).
association <- function(x, x_type, target, target_type) {
  if (x_type == "quanti" && target_type == "quanti") {
    res <- quanti_quanti(x, target)
    return(list(index = "r de Pearson", value = res$r %||% NA_real_, n = res$n %||% 0L))
  }
  if (x_type == "quali" && target_type == "quali") {
    res <- chi2_indices(contingency_table(x, target))
    return(list(index = "C de Cramér", value = res$cramer %||% NA_real_, n = res$n %||% 0L))
  }
  res <- if (x_type == "quanti") correlation_ratio(x, target) else correlation_ratio(target, x)
  list(index = "Rapport de corrélation c", value = res$c %||% NA_real_, n = res$n %||% 0L)
}

ASSOCIATION_INDICES <- c("r de Pearson", "Rapport de corrélation c", "C de Cramér")

# One row per quanti or quali variable other than the target, ordered by index
# (fixed order, independent of the locale) then by decreasing absolute value.
# The indices are not on the same scale.
association_table <- function(dataset, target) {
  types <- dataset$var_types
  vars <- setdiff(names(types)[types %in% c("quanti", "quali")], target)
  rows <- lapply(vars, function(v) {
    a <- association(dataset$data[[v]], types[[v]], dataset$data[[target]], types[[target]])
    data.frame(variable = v, type = types[[v]], index = a$index, value = a$value, n = a$n)
  })
  table <- do.call(rbind, c(list(data.frame(variable = character(0), type = character(0),
                                            index = character(0), value = numeric(0),
                                            n = integer(0))), rows))
  index_rank <- match(table$index, ASSOCIATION_INDICES)
  table <- table[order(index_rank, -abs(table$value), na.last = TRUE), , drop = FALSE]
  rownames(table) <- NULL
  table
}
