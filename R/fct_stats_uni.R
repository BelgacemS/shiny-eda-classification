# Univariate statistics of the course (slides 01 and 02), on the non-missing
# values of one variable. Frequencies are proportions; the interface shows %.

# A quanti variable with at most this many distinct values is treated as
# discrete (table by value), as in the course; above, the table is by classes.
MAX_DISCRETE_VALUES <- 20

CLASS_RULES <- c("sturges", "scott", "fd")

# Upper bound on the number of classes: Freedman-Diaconis with a small IQR and
# an extreme value would otherwise ask for millions of classes.
MAX_CLASSES <- 200

# Table by value: x_i in increasing order, n_i, N_i, f_i, F_i.
freq_table_values <- function(x) {
  x <- x[!is.na(x)]
  values <- sort(unique(x))
  n <- tabulate(match(x, values), nbins = length(values))
  cumulative_table(data.frame(value = values), n)
}

# Class boundaries for a rule, as hist() computes them: the number of classes
# comes from nclass.Sturges, nclass.scott or nclass.FD, capped at MAX_CLASSES,
# then pretty() rounds the boundaries. Scott and FD need a spread: with a single
# value or a zero spread they give no number, and Sturges is used instead.
class_breaks <- function(x, rule = "sturges") {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(numeric(0))
  }
  rule_fun <- switch(rule,
    sturges = grDevices::nclass.Sturges,
    scott = grDevices::nclass.scott,
    fd = grDevices::nclass.FD,
    stop("règle de classes inconnue : ", rule, call. = FALSE)
  )
  # nclass.scott() fails on a single value (its sd is NA).
  k <- tryCatch(suppressWarnings(rule_fun(x)), error = function(e) NA)
  if (length(k) != 1 || !is.finite(k) || k < 1) k <- grDevices::nclass.Sturges(x)
  pretty(range(x), n = min(k, MAX_CLASSES), min.n = 1)
}

# Table by class ]b_{i-1}, b_i] (the first class also contains b_0): bounds,
# centre x_i = (b_{i-1} + b_i) / 2, width a_i, n_i, N_i, f_i, F_i and the
# frequency density f_i / a_i that the histogram draws.
# Counts come from hist(), not cut(): a pretty() boundary such as 97.2 may be
# stored as 97.19999999999999, and hist() widens the boundaries by a tiny fuzz
# so that a value read as 97.2 still falls in ]97.1, 97.2].
freq_table_classes <- function(x, breaks) {
  x <- x[!is.na(x)]
  n <- if (length(x) == 0) integer(length(breaks) - 1) else
    as.integer(graphics::hist(x, breaks = breaks, plot = FALSE)$counts)
  table <- data.frame(
    lower = breaks[-length(breaks)],
    upper = breaks[-1]
  )
  table$center <- (table$lower + table$upper) / 2
  table$width <- table$upper - table$lower
  table <- cumulative_table(table, n)
  table$density <- table$f / table$width
  table
}

cumulative_table <- function(table, n) {
  total <- sum(n)
  table$n <- n
  table$N <- cumsum(n)
  table$f <- if (total > 0) n / total else rep(NA_real_, length(n))
  table$F <- cumsum(table$f)
  table
}

# Centre and dispersion indicators. Quartiles come from quantile(type = 7),
# those of geom_boxplot; variance and standard deviation are in 1/n.
uni_indicators <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), type = 7, names = FALSE)
  list(
    n = length(x),
    mean = mean_1n(x),
    median = q[2],
    q1 = q[1],
    q3 = q[3],
    min = min(x),
    max = max(x),
    range = max(x) - min(x),
    iqr = q[3] - q[1],
    variance = var_1n(x),
    sd = sd_1n(x)
  )
}

# Table of a quali variable: every level in factor order, with n_i and f_i.
freq_table_levels <- function(x) {
  x <- as.factor(x)
  n <- as.integer(table(x))
  total <- sum(n)
  data.frame(level = levels(x), n = n,
             f = if (total > 0) n / total else rep(NA_real_, length(n)))
}

is_discrete <- function(x) {
  length(unique(x[!is.na(x)])) <= MAX_DISCRETE_VALUES
}
