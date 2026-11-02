# Without NOT_CRAN, testthat turns a missing package into a skip instead of a failure.
Sys.setenv(NOT_CRAN = "true")

# Shiny sources R/ automatically when the app starts, testthat does not.
for (f in list.files(test_path("..", "..", "R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}
