test_that("the app builds without error", {
  app <- shiny::shinyAppDir(test_path("..", ".."))
  expect_s3_class(app, "shiny.appobj")
})
