context("predict_pmml")

test_that("predict_pmml() returns correct prediction for regression models", {
  skip_on_cran()
  expect_equivalent(predict_pmml(iris[42, ], "iris_model")$outputs[1, ],
                    predict(iris_fit, iris[42, ]))
  expect_equivalent(predict_pmml(iris[93, ], "iris_model")$outputs[1, ],
                    predict(iris_fit, iris[93, ]))
})

test_that("predict_pmml() returns correct prediction for binary classififcation models", {
  skip_on_cran()
  server_prediction <- predict_pmml(kyphosis[38, ], "kyphosis_model")

  expect_equal(server_prediction$outputs[, 3],
               as.character(predict(kyphosis_fit, kyphosis[38, ], type = "class")))
  expect_equal(server_prediction$outputs[, 1:2] %>%
                 as.double %>%
                 sort,
               predict(kyphosis_fit, newdata = kyphosis[38, ]) %>%
                 as.double %>%
                 sort)
})

test_that("predict_pmml() returns error if 'model_name' is not length one character vector", {
  skip_on_cran()
  expect_error(predict_pmml(iris[1, ], c("iris_model", "kyphosis_model")))
  expect_error(predict_pmml(unlist(iris[1, ]), 1:2))
})

test_that("predict_pmml() returns error if 'x' is not a one row data frame", {
  skip_on_cran()
  expect_error(predict_pmml(unlist(iris[1, ]), "iris_model"))
  expect_error(predict_pmml(iris[1:2, ], "iris_model"))
})

test_that("predict_pmml() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(predict_pmml(iris[1, ], "unknown_model"), err_not_known, fixed = TRUE)
})
