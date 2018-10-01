context("full_model_operations checks")

test_that("upload_model() returns a list after model upload", {
  expect_equal(upload_model("kyphosis_pmml.xml"), list(model_name = "kyphosis_model",
                                                       is_active = TRUE))
  expect_equal(upload_model("iris_pmml.xml"), list(model_name = "iris_model",
                                                   is_active = TRUE))
})

test_that("upload_model(): model names must be unique; file must exist", {
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

test_that("get_models() returns a vector with model names", {
  my_models <- zementisr::get_models()
  my_models <- my_models[c(length(my_models)- 1, length(my_models))]

  expect_length(my_models, 2)
  expect_equal(my_models, c("iris_model", "kyphosis_model"))
})

test_that("deactivate_model() returns list after successful deactivation", {
  expect_equal(deactivate_model("iris_model"), list(model_name = "iris_model",
                                                    is_active = FALSE))
  expect_equal(deactivate_model("kyphosis_model"), list(model_name = "kyphosis_model",
                                                        is_active = FALSE))
})

test_that("deactivate_model() returns error if model name is unknown to the server", {
  expect_error(activate_model("unknown_model"), err_not_known, fixed = TRUE)
})

test_that("activate_model() returns list after successful activation", {
  expect_equal(activate_model("iris_model"), list(model_name = "iris_model",
                                                  is_active = TRUE))
  expect_equal(activate_model("kyphosis_model"), list(model_name = "kyphosis_model",
                                                  is_active = TRUE))
})

test_that("activate_model() returns error if model name is unknown to the server", {
  expect_error(activate_model("unknown_model"), err_not_known, fixed = TRUE)
})

