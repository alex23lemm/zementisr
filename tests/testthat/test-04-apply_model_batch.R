context("apply_model_batch")

iris_resp <- list(
  model = "iris_model",
  outputs = data.frame(
    Predicted_Sepal.Length = c(5.004788,
                               4.756844,
                               4.773097)
  ))

kyphosis_resp <- list(
  model = "kyphosis_model",
  outputs = data.frame(
    Probability_present = c(0.5789474, 0.1428571, 0.5789474),
    Probability_absent = c(0.4210526, 0.8571429, 0.4210526),
    Predicted_Kyphosis = c("present", "absent", "present"),
    stringsAsFactors = FALSE
  )
)

test_that("apply_model_batch works for data frames", {
  expect_equal(apply_model_batch(iris[1:3, ], "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(apply_model_batch(kyphosis[1:3, ], "kyphosis_model"),
               kyphosis_resp, tolerance = 1.5e-6)
})

test_that("apply_model_batch() works for .csv files", {
  expect_equal(apply_model_batch("iris.csv", "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(apply_model_batch("kyphosis.csv", "kyphosis_model"), kyphosis_resp,
               tolerance = 1.5e-6)
})

test_that("apply_model_batch() works for .json files", {
  expect_equal(apply_model_batch("iris.json", "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(apply_model_batch("kyphosis.json", "kyphosis_model"), kyphosis_resp,
               tolerance = 1.5e-6)

})

test_that("apply_model_batch() works for .zip files", {
  expect_equal(apply_model_batch("iris.csv.zip", "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(apply_model_batch("kyphosis.json.zip", "kyphosis_model"), kyphosis_resp,
               tolerance = 1.5e-6)

})

test_that("apply_model_batch() requires data frame or valid path to a file as input", {
  expect_error(apply_model_batch("unknown.csv", "iris_model"),
               "Please either provide a data frame or a path to a file",
               fixed = TRUE)
  expect_error(apply_model_batch(1:4, "iris_model"),
               "Please either provide a data frame or a path to a file",
               fixed = TRUE)
})

test_that("apply_model_batch() returns error if 'model_name' is not length one character vector", {
  expect_error(apply_model_batch(iris[1:2, ], c("iris_model", "kyphosis_model")))
  expect_error(apply_model_batch(iris[1:2, ], 1:2))
})

test_that("apply_model_batch() returns error if model name is unknown to the server", {
  expect_error(apply_model_batch(iris[1:3, ], "unknown_model"), err_not_known, fixed = TRUE)
})
