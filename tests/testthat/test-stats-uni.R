# The 48 ages of slide 4 of course 01 (S. Affeldt), copied from the slide.
AGES_48 <- c(43, 29, 57, 45, 50, 29, 37, 59, 46, 31, 46, 24, 33, 38, 49, 31,
             62, 60, 52, 38, 38, 26, 41, 52, 60, 49, 52, 41, 38, 26, 37, 59,
             57, 41, 29, 33, 33, 43, 46, 57, 46, 33, 46, 49, 57, 57, 46, 43)

test_that("indicators of the 48 ages match the course, in 1/n", {
  ind <- uni_indicators(AGES_48)
  expect_equal(ind$n, 48L)
  expect_equal(ind$mean, 43.625)
  # Variance 109.776 and sd 10.477 in 1/n; the sd in n - 1 would be 10.588.
  expect_equal(round(ind$variance, 3), 109.776)
  expect_equal(round(ind$sd, 3), 10.477)
  expect_equal(round(sqrt(ind$variance * 48 / 47), 3), 10.588)
  expect_equal(c(ind$min, ind$max, ind$range), c(24, 62, 38))
  # quantile(type = 7) gives Q1 = 36; the slide of course 02 gives 35 (middle of
  # 33 and 37, where F_i is exactly 25 %). Median 44 and Q3 52 in both.
  expect_equal(c(ind$q1, ind$median, ind$q3), c(36, 44, 52))
  expect_equal(ind$iqr, 16)
})

test_that("the table by value of the 48 ages is the one of the slide", {
  t <- freq_table_values(AGES_48)
  expect_equal(t$value, c(24, 26, 29, 31, 33, 37, 38, 41, 43, 45, 46, 49, 50, 52, 57, 59, 60, 62))
  expect_equal(t$n, c(1, 2, 3, 2, 4, 2, 4, 3, 3, 1, 6, 3, 1, 3, 5, 2, 2, 1))
  expect_equal(t$N, cumsum(t$n))
  expect_equal(tail(t$N, 1), 48L)
  expect_equal(round(100 * t$f, 2)[c(1, 11, 15)], c(2.08, 12.50, 10.42))
  # The slide prints F = 72,91 for 50 by adding rounded f_i; the exact value is 35/48.
  expect_equal(t$F[t$value == 50], 35 / 48)
  expect_equal(tail(t$F, 1), 1)
})

test_that("the table by class counts ]b_{i-1}, b_i] as hist() does", {
  breaks <- class_breaks(AGES_48, "sturges")
  expect_equal(breaks, seq(20, 65, by = 5))
  t <- freq_table_classes(AGES_48, breaks)
  # Counted by hand: [20,25] 24; ]25,30] 26 26 29 29 29; ]30,35] 31 31 33 33 33 33; ...
  expect_equal(t$n, c(1, 5, 6, 6, 7, 10, 3, 9, 1))
  expect_equal(t$center[1:2], c(22.5, 27.5))
  expect_equal(sum(t$density * t$width), 1)
  expect_equal(t$n, graphics::hist(AGES_48, breaks = breaks, plot = FALSE)$counts)
  # A value equal to an inner boundary goes to the lower class, b_0 to the first class.
  t <- freq_table_classes(c(0.8, 1.0, 1.1, 1.2, NA), c(0.8, 1.0, 1.2))
  expect_equal(t$n, c(2, 2))
})

test_that("the class rules give the number of classes of nclass.* before pretty()", {
  set.seed(4)
  x <- rexp(500)
  for (rule in CLASS_RULES) {
    breaks <- class_breaks(x, rule)
    expect_true(min(breaks) <= min(x) && max(breaks) >= max(x))
    k <- switch(rule, sturges = grDevices::nclass.Sturges(x), scott = grDevices::nclass.scott(x),
                fd = grDevices::nclass.FD(x))
    expect_equal(breaks, pretty(range(x), n = k, min.n = 1))
  }
  expect_error(class_breaks(x, "rice"), "inconnue")
})

test_that("a quali table keeps every level in factor order", {
  t <- freq_table_levels(factor(c("b", "a", "b", NA), levels = c("b", "a", "c")))
  expect_equal(t$level, c("b", "a", "c"))
  expect_equal(t$n, c(2L, 1L, 0L))
  expect_equal(t$f, c(2 / 3, 1 / 3, 0))
})

test_that("discrete means at most 20 distinct values, and empty input is handled", {
  expect_true(is_discrete(AGES_48))
  expect_false(is_discrete(1:21))
  expect_null(uni_indicators(c(NA_real_, NA)))
  expect_equal(nrow(freq_table_values(numeric(0))), 0L)
})

test_that("decimals_needed shows each value without trailing zeros", {
  expect_equal(decimals_needed(c(22.5, 27.5)), 1)
  expect_equal(decimals_needed(c(24, 26)), 0)
  expect_equal(decimals_needed(c(0.9, 1.25, NA)), 2)
})

test_that("the univariate module lists quanti and quali variables and renders every output", {
  ds <- make_dataset(
    data.frame(age = c(AGES_48, NA, NA), grp = rep(c("a", "b"), 25),
               code = sprintf("c%02d", 1:50)),
    c(age = "quanti", grp = "quali", code = "id")
  )
  shiny::testServer(mod_explore_uni_server, args = list(dataset_clean = shiny::reactive(ds)), {
    session$flushReact()
    expect_equal(analysed(), c("age", "grp"))
    session$setInputs(var = "age", table_mode = "auto", class_rule = "scott",
                      hist_scale = "density", show_density = TRUE)
    expect_equal(output$var_type, "quanti")
    expect_match(output$excluded, "48 valeurs analysées, 2 manquantes exclues")
    expect_match(output$indicators, "43,625")
    expect_match(output$indicators, "109,776")
    expect_false(use_classes())
    for (plot in c("bars", "ecdf", "boxplot", "histogram", "cumulative_curve")) {
      expect_true(nchar(output[[plot]]$src) > 0)
    }
    session$setInputs(table_mode = "classes")
    expect_true(use_classes())
    expect_true(nchar(output$quanti_table) > 0)
    session$setInputs(var = "grp")
    expect_equal(output$var_type, "quali")
    expect_equal(quali_table()$n, c(25L, 25L))
    for (plot in c("columns", "pie")) expect_true(nchar(output[[plot]]$src) > 0)
  })
})
