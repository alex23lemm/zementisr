context("model_operations")

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
