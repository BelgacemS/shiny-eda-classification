# Descriptive statistics with the 1/n convention of the course.
# Base var(), sd() and cov() divide by n - 1 and must not be used for descriptive output.
# NA values propagate: callers work on complete cases.

mean_1n <- function(x) {
  sum(x) / length(x)
}

var_1n <- function(x) {
  mean_1n((x - mean_1n(x))^2)
}

sd_1n <- function(x) {
  sqrt(var_1n(x))
}

cov_1n <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length.", call. = FALSE)
  }
  mean_1n((x - mean_1n(x)) * (y - mean_1n(y)))
}

# Constancy is read on the values, never on a spread: the 1/n variance of a
# constant decimal column such as rep(0.1, 50) is about 1e-33 after rounding,
# not 0, and would pass a "> 0" test. x: values without NA.
is_constant <- function(x) {
  length(x) == 0 || max(x) == min(x)
}
