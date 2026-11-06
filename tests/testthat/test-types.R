course_file <- function(name) test_path("fixtures", "course", name)

test_that("students.csv columns get the expected types", {
  path <- course_file("students.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  types <- detect_types(read_table_file(path))
  expect_equal(as.vector(types[c("Sex", "Major", "Level", "BloodType")]), rep("quali", 4))
  expect_equal(as.vector(types[c("Height", "Sleep", "MilesHome")]), rep("quanti", 3))
})

test_that("chiens.csv has 4 quanti columns", {
  path <- course_file("chiens.csv")
  skip_if_not(file.exists(path), "course CSV not available")
  types <- detect_types(read_table_file(path))
  expect_equal(as.vector(types), rep("quanti", 4))
})

test_that("character identifiers and high-cardinality columns are typed id", {
  data <- data.frame(
    code = sprintf("C%03d", 1:60),
    city = rep(sprintf("V%02d", 1:55), length.out = 60),
    group = rep(c("a", "b", "c"), 20),
    flag = rep(c(TRUE, FALSE), 30)
  )
  types <- detect_types(data)
  expect_equal(as.vector(types), c("id", "id", "quali", "quali"))
})

test_that("an integer column with distinct values stays quanti with an alert", {
  data <- data.frame(row = 1:10, x = c(1.5, 2:10), y = rep(1:5, 2))
  types <- detect_types(data)
  expect_equal(as.vector(types), rep("quanti", 3))
  expect_equal(attr(types, "possible_id"), "row")
})

test_that("missing values are ignored when counting distinct values", {
  data <- data.frame(g = c("a", "a", NA, "b"), single = c(NA, NA, NA, "z"))
  types <- detect_types(data)
  expect_equal(as.vector(types), c("quali", "quali"))
})

test_that("unsupported classes are ignored", {
  data <- data.frame(day = as.Date("2024-01-01") + 0:2)
  expect_equal(as.vector(detect_types(data)), "ignore")
})
