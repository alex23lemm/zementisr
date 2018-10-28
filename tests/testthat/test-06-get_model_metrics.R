context("test-06-get_model_metrics")

test_that("get_model_metrics() returns list with prediction and memory metrics", {
  skip_on_cran()
  metrics_rsp <- get_model_metrics("iris_model")

  expect_length(metrics_rsp, 2)
  expect_named(metrics_rsp, c("prediction_metrics", "memory_metrics"), ignore.order = TRUE)
  expect_is(metrics_rsp[["prediction_metrics"]], "data.frame")
  expect_is(metrics_rsp[["memory_metrics"]], "data.frame")
})

test_that("get_model_metrics() only returns list of memory metrics", {
  iris_lm <- lm(Sepal.Length ~ ., data = iris)
  iris_tmp <- pmml::pmml(iris_lm, model.name = "iris_tmp")
  upload_model(iris_tmp)
  metrics_rsp <- get_model_metrics("iris_tmp")
  delete_model("iris_tmp")

  expect_length(metrics_rsp, 1)
  expect_named(metrics_rsp, c("memory_metrics"))
  expect_is(metrics_rsp[["memory_metrics"]], "data.frame")
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
