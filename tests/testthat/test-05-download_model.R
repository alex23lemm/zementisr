context("test-05-download_model")

test_that("download_model() downloads model source and returns a XMLInternalDocument object", {
  skip_on_cran()
  iris_download <- download_model("iris_model")

  expect_length(iris_download, 2)
  expect_is(iris_download[["model_name"]], "character")
  expect_s3_class(iris_download[["model_source"]], "XMLInternalDocument")
})

test_that("download_model() returns error if 'model_name' is not length one character vector", {
  skip_on_cran()
  expect_error(download_model(c("iris_model", "kyphosis_model")))
  expect_error(download_model(1:2))
})

test_that("download_model() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(download_model("unknown_model"), err_not_known, fixed = TRUE)
})
