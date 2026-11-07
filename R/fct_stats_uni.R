# Univariate statistics of the course (slides 01 and 02), on the non-missing
# values of one variable. Frequencies are proportions; the interface shows %.

# A quanti variable with at most this many distinct values is treated as
# discrete (table by value), as in the course; above, the table is by classes.
MAX_DISCRETE_VALUES <- 20

CLASS_RULES <- c("sturges", "scott", "fd")

# Table by value: x_i in increasing order, n_i, N_i, f_i, F_i.
freq_table_values <- function(x) {
  x <- x[!is.na(x)]
  values <- sort(unique(x))
  n <- tabulate(match(x, values), nbins = length(values))
  cumulative_table(data.frame(value = values), n)
}

# Class boundaries of hist() for a rule: the number of classes comes from
# nclass.Sturges, nclass.scott or nclass.FD, then pretty() rounds the boundaries.
class_breaks <- function(x, rule = "sturges") {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(numeric(0))
  }
  breaks <- switch(rule, sturges = "Sturges", scott = "Scott", fd = "FD",
                   stop("règle de classes inconnue : ", rule, call. = FALSE))
  graphics::hist(x, breaks = breaks, plot = FALSE)$breaks
}

# Table by class ]b_{i-1}, b_i] (the first class also contains b_0), as hist():
# bounds, centre x_i = (b_{i-1} + b_i) / 2, width a_i, n_i, N_i, f_i, F_i and
# the frequency density f_i / a_i that the histogram draws.
freq_table_classes <- function(x, breaks) {
  x <- x[!is.na(x)]
  k <- length(breaks) - 1
  classes <- cut(x, breaks, include.lowest = TRUE, right = TRUE)
  n <- as.integer(tabulate(as.integer(classes), nbins = k))
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
