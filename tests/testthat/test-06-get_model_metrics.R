context("test-06-get_model_metrics")

test_that("get_model_metrics() returns list with prediction and memory metrics", {
  skip_on_cran()
  metrics_rsp <- get_model_metrics("iris_model")

  expect_length(metrics_rsp, 3)
  expect_named(metrics_rsp, c("model_name", "prediction_metrics", "memory_metrics"), ignore.order = TRUE)
  expect_equal(metrics_rsp[["model_name"]], "iris_model")
  expect_is(metrics_rsp[["prediction_metrics"]], "data.frame")
  expect_is(metrics_rsp[["memory_metrics"]], "data.frame")
})

test_that("get_model_metrics() only returns list of memory metrics", {
  skip_on_cran()
  iris_lm <- lm(Sepal.Length ~ ., data = iris)
  iris_tmp <- pmml::pmml(iris_lm, model.name = "iris_tmp")
  upload_model(iris_tmp)
  metrics_rsp <- get_model_metrics("iris_tmp")
  delete_model("iris_tmp")

  expect_length(metrics_rsp, 2)
  expect_named(metrics_rsp, c("model_name", "memory_metrics"), ignore.order = TRUE)
  expect_equal(metrics_rsp[["model_name"]], "iris_tmp")
  expect_is(metrics_rsp[["memory_metrics"]], "data.frame")
})

test_that("get_model_metrics() only returns model name", {
  skip_on_cran()
  deactivate_model("iris_model")
  metrics_rsp <- get_model_metrics("iris_model")
  activate_model("iris_model")

  expect_length(metrics_rsp, 1)
  expect_named(metrics_rsp, "model_name")
  expect_equal(metrics_rsp[["model_name"]], "iris_model")
})

test_that("get_model_metrics() returns error if 'model_name' is not length one character vector", {
  skip_on_cran()
  expect_error(get_model_metrics(c("iris_model", "kyphosis_model")))
  expect_error(get_model_metrics(42))
})

test_that("get_model_metrics() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(get_model_metrics("unknown_model"), err_not_known, fixed = TRUE)
})
