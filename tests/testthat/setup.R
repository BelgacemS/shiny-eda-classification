# Shiny sources R/ automatically when the app starts, testthat does not.
for (f in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
