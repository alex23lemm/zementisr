context("predict_pmml_batch")

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

test_that("predict_pmml_batch() works for data frames", {
  skip_on_cran()
  expect_equal(predict_pmml_batch(iris[1:3, ], "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(predict_pmml_batch(iris[1:3, ], "iris_model", max_threads = 1,
                                  max_records_per_thread =10), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(predict_pmml_batch(kyphosis[1:3, ], "kyphosis_model"),
               kyphosis_resp, tolerance = 1.5e-6)
})

test_that("predict_pmml_batch() works for .csv files", {
  skip_on_cran()
  expect_equal(predict_pmml_batch("iris.csv", "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(predict_pmml_batch("kyphosis.csv", "kyphosis_model"), kyphosis_resp,
               tolerance = 1.5e-6)
})

test_that("predict_pmml_batch() works for .json files", {
  skip_on_cran()
  expect_equal(predict_pmml_batch("iris.json", "iris_model"), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(predict_pmml_batch("kyphosis.json", "kyphosis_model"), kyphosis_resp,
               tolerance = 1.5e-6)
})

test_that("predict_pmml_batch() works for .zip files", {
  skip_on_cran()
  # Download compressed iris predictions
  tmp_iris_z <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_iris_z))
  predict_pmml_batch("iris.csv.zip", "iris_model", tmp_iris_z)
  tmp_iris_uz <- unzip(tmp_iris_z)
  on.exit(unlink(tmp_iris_uz), add = TRUE)

  # Download compressed kyphosis predictions
  tmp_kyphosis_z <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_kyphosis_z), add = TRUE)
  predict_pmml_batch("kyphosis.json.zip", "kyphosis_model", tmp_kyphosis_z)
  tmp_kyphosis_uz <- unzip(tmp_kyphosis_z)
  on.exit(unlink(tmp_kyphosis_uz), add = TRUE)

  expect_equal(jsonlite::fromJSON(tmp_iris_uz), iris_resp,
               tolerance = 1.5e-6)
  expect_equal(jsonlite::fromJSON(tmp_kyphosis_uz), kyphosis_resp,
               tolerance = 1.5e-6)
})

test_that("predict_pmml_batch() requires data frame or valid path to a file as input", {
  skip_on_cran()
  expect_error(predict_pmml_batch("unknown.csv", "iris_model"),
               "Please either provide a data frame or a path to a file",
               fixed = TRUE)
  expect_error(predict_pmml_batch(1:4, "iris_model"),
               "Please either provide a data frame or a path to a file",
               fixed = TRUE)
})

test_that("predict_pmml_batch() returns error if path for compressed file predictions is missing", {
  skip_on_cran()
  expect_error(predict_pmml_batch("iris.csv.zip", "iris_model"),
               "Please provide a 'path' to a file to which the predictions from Zementis Server are written to.",
               fixed = TRUE)
})

test_that("predict_pmml_batch() returns error if 'max_threads' argument is not length one numeric vector", {
  skip_on_cran()
  expect_error(predict_pmml_batch(iris[1:2, ], "iris_model", max_threads = "bigwhoop"))
  expect_error(predict_pmml_batch(iris[1:2, ], "iris_model", max_threads = c(1, 2)))
})

test_that("predict_pmml_batch() returns error if 'max_records_per_thread' argument is not length one numeric vector", {
  skip_on_cran()
  expect_error(predict_pmml_batch(iris[1:2, ], "iris_model", max_records_per_thread = "bigwhoop"))
  expect_error(predict_pmml_batch(iris[1:2, ], "iris_model", max_records_per_thread = c(2, 5)))
})

test_that("predict_pmml_batch() returns error if 'model_name' is not length one character vector", {
  skip_on_cran()
  expect_error(predict_pmml_batch(iris[1:2, ], c("iris_model", "kyphosis_model")))
  expect_error(predict_pmml_batch(iris[1:2, ], 1:2))
})

test_that("predict_pmml_batch() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(predict_pmml_batch(iris[1:3, ], "unknown_model"), err_not_known, fixed = TRUE)
})
