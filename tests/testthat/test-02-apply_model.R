context("02-apply_model")

test_that("apply_model() returns correct prediction for regression models", {
  expect_equivalent(apply_model("iris_model", iris[42, ])$outputs[1, ],
                    predict(iris_fit, iris[42, ]))
  expect_equivalent(apply_model("iris_model", iris[93, ])$outputs[1, ],
                    predict(iris_fit, iris[93, ]))
})

test_that("apply_model() returns correct prediction for classififcation models", {
  server_prediction <- apply_model("kyphosis_model", kyphosis[38, ])

  expect_equal(server_prediction$outputs[, 3],
               as.character(predict(kyphosis_fit, kyphosis[38, ], type = "class")))
  expect_equal(server_prediction$outputs[, 1:2] %>%
                 as.double %>%
                 sort,
               predict(kyphosis_fit, newdata = kyphosis[38, ]) %>%
                 as.double %>%
                 sort)
})

