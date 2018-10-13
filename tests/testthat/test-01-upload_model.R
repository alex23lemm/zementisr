context("upload_model")

test_that("upload_model() works for XMLNode objects", {
  skip_on_travis()
  expect_equal(upload_model(iris_pmml), list(model_name = "iris_model",
                                             is_active = TRUE))
})

test_that("upload_model() works for pmml files", {
  skip_on_travis()
  expect_equal(upload_model("kyphosis_pmml.xml"), list(model_name = "kyphosis_model",
                                                       is_active = TRUE))
})

test_that("upload_model(): model names must be unique; file must exist", {
  skip_on_travis()
  iris_upload_err <- paste("Zementis Server API request failed [409]",
                           "Client error",
                           "Conflict",
                           "Client error: (409) Conflict",
                           "A model with the name 'iris_model' already exists.",
                           sep = "\n")
  expect_error(upload_model("iris_pmml.xml"), iris_upload_err, fixed = TRUE)
  expect_error(upload_model("kyphosis_pmml.xml"))
  expect_error(upload_model("bigwhoop.xml"))
})
