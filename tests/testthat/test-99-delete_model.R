context("delete_model")

test_that("delete_model() returns character vector", {
  expect_is(delete_model("kyphosis_model"), "character")
  expect_gte(length(delete_model("iris_model")), 0)
})

test_that("delete_model() returns error if 'model_name' is not length one character vector", {
  expect_error(delete_model(c("iris_model", "kyphosis_model")))
  expect_error(delete_model(1:2))
})

test_that("delete_model() returns error if model name is unknown to the server", {
  expect_error(delete_model("unknown_model"), err_not_known, fixed = TRUE)
})
