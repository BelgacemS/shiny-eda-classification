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
