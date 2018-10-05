context("apply_model")

test_that("apply_model() returns correct prediction for regression models", {
  expect_equivalent(apply_model(iris[42, ], "iris_model")$outputs[1, ],
                    predict(iris_fit, iris[42, ]))
  expect_equivalent(apply_model(iris[93, ], "iris_model")$outputs[1, ],
                    predict(iris_fit, iris[93, ]))
})

test_that("apply_model() returns correct prediction for classififcation models", {
  server_prediction <- apply_model(kyphosis[38, ], "kyphosis_model")

  expect_equal(server_prediction$outputs[, 3],
               as.character(predict(kyphosis_fit, kyphosis[38, ], type = "class")))
  expect_equal(server_prediction$outputs[, 1:2] %>%
                 as.double %>%
                 sort,
               predict(kyphosis_fit, newdata = kyphosis[38, ]) %>%
                 as.double %>%
                 sort)
})

test_that("apply_model() returns correct prediction for binary classififcation models", {
  server_prediction <- apply_model(kyphosis[38, ], "kyphosis_model")

  expect_equal(server_prediction$outputs[, 3],
               as.character(predict(kyphosis_fit, kyphosis[38, ], type = "class")))
  expect_equal(server_prediction$outputs[, 1:2] %>%
                 as.double %>%
                 sort,
               predict(kyphosis_fit, newdata = kyphosis[38, ]) %>%
                 as.double %>%
                 sort)
})

test_that("apply_model() returns error if model name is unknown to the server", {
  expect_error(apply_model(iris[1, ], "unknown_model"), err_not_known, fixed = TRUE)
})
