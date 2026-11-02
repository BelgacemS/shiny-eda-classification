testthat::local_edition(3)

test_that("the app builds and its server starts", {
  app <- shiny::shinyAppDir(test_path("..", ".."))
  expect_s3_class(app, "shiny.appobj")
  shiny::testServer(app, expect_true(TRUE))
})
