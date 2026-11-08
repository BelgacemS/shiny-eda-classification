# Extracted from test-models.R:354

# prequel ----------------------------------------------------------------------
simulated <- function(n = 400, seed = 1) {
  withr::local_seed(seed)
  x <- rnorm(n)
  g <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  y <- rbinom(n, 1, stats::plogis(1.5 * x + (g == "b")))
  list(x = data.frame(x = x, g = g), y = y)
}
pipeline_data <- function(n_acc = 300, seed = 12) {
  withr::local_seed(seed)
  acc <- rep(seq_len(n_acc), each = 3)
  n <- length(acc)
  x <- rnorm(n)
  data <- data.frame(
    y = factor(ifelse(stats::rbinom(n, 1, stats::plogis(2 * x - 1)) == 1, "oui", "non")),
    x = x, z = rnorm(n, 100, 10), g = factor(sample(c("a", "b", "c"), n, TRUE)),
    acc = as.character(acc), year = rep(sample(2019:2024, n_acc, TRUE), each = 3))
  make_dataset(data, c(y = "quali", x = "quanti", z = "quanti", g = "quali", acc = "id",
                       year = "quanti"))
}
pipeline_settings <- function(...) {
  utils::modifyList(list(target = "y", positive = "oui", predictors = c("x", "z", "g"),
                         sample_size = 10000, group = "acc", mode = "grouped", test_prop = 0.2,
                         valid_prop = 0.25, models = c("glm", "ranger"), seed = 1), list(...))
}

# test -------------------------------------------------------------------------
withr::local_seed(15)
n <- 800
x <- data.frame(a = rnorm(n), f = factor(sample(c("p", "q", "r"), n, TRUE)),
                  b = runif(n))
x$a2 <- 2 * x$a + 1
x$g <- factor(ifelse(x$f == "p", "P", "autre"))
y <- stats::rbinom(n, 1, stats::plogis(x$a + (x$f == "q")))
new <- x[sample(n, 200), ]
for (w in list(NULL, stats::runif(n, 0.2, 3))) {
    model <- fit_model("glm", x, y, weights = w)
    expect_true(anyNA(stats::coef(model$fit)))
    p <- ncol(model$fit$qr$qr)
    expect_equal(nrow(model$fit$qr$qr), p)
    # Reference: the same fit by glm() with the same reference levels, full QR kept.
    data <- x
    for (v in names(model$levels)) data[[v]] <- factor(data[[v]], levels = model$levels[[v]])
    data$..y <- y
    ref_w <- if (is.null(w)) rep(1, n) else w
    family <- if (is.null(w)) stats::binomial() else stats::quasibinomial()
    reference <- stats::glm(..y ~ ., data = data, family = family, weights = ref_w)
    expect_gt(nrow(reference$qr$qr), p)
    newdata <- new
    for (v in names(model$levels)) newdata[[v]] <- factor(newdata[[v]], levels = model$levels[[v]])
    expected <- unname(suppressWarnings(stats::predict(reference, newdata = newdata, type = "response")))
    expect_identical(suppressWarnings(predict_proba(model, new)), expected)
    expect_equal(stats::confint.default(model$fit), stats::confint.default(reference))
  }
