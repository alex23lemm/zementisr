context("model_operations")

test_that("get_models() returns a vector with model names", {
  skip_on_cran()
  my_models <- zementisr::get_models()

  creation_dates <- my_models %>%
    purrr::map(get_model_properties) %>%
    purrr::map_chr("creationDate") %>%
    as.POSIXct

  my_models <- my_models[order(creation_dates, decreasing = TRUE)][1:2]

  expect_length(my_models, 2)
  expect_equal(my_models, c("iris_model", "kyphosis_model"))
})

test_that("get_model_properties() returns model properties", {
  skip_on_cran()
  iris_props <- list(
    modelName = "iris_model",
    description = "Linear Regression Model",
    isActive = TRUE,
    inputFields = data.frame(
      name = c("Sepal.Width", "Petal.Length", "Petal.Width", "Species"),
      type = c("DOUBLE", "DOUBLE", "DOUBLE", "STRING"),
      usage = c("ACTIVE", "ACTIVE", "ACTIVE", "ACTIVE"),
      stringsAsFactors = FALSE),
    outputFields = data.frame(
      name = "Predicted_Sepal.Length",
      type = "DOUBLE",
      usage = "OUTPUT",
      stringsAsFactors = FALSE)
  )

  order_by_column <- function(df) {
    df[, order(names(df))]
  }
  iris_rsp <- get_model_properties("iris_model") %>%
    purrr::map(., function(.x) {
      if (class(.x) == "data.frame")
        order_by_column(.x)
      else
        .x
      })

  expect_named(get_model_properties("kyphosis_model"),
               c("modelName", "description", "creationDate", "isActive", "inputFields", "outputFields"))
  iris_rsp$creationDate <- NULL
  expect_equal(iris_rsp, iris_props)
})

test_that("get_model_properties() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(get_model_properties("unknown_model"), err_not_known, fixed = TRUE)
})

test_that("deactivate_model() returns list after successful deactivation", {
  skip_on_cran()
  expect_equal(deactivate_model("iris_model"), list(model_name = "iris_model",
                                                    is_active = FALSE))
  expect_equal(deactivate_model("kyphosis_model"), list(model_name = "kyphosis_model",
                                                        is_active = FALSE))
})

test_that("deactivate_model() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(deactivate_model("unknown_model"), err_not_known, fixed = TRUE)
})

test_that("deactivate_model() returns error if 'model_name' is not length one character vector", {
  skip_on_cran()
  expect_error(deactivate_model(c("iris_model", "kyphosis_model")))
  expect_error(deactivate_model(1:2))
})

test_that("activate_model() returns list after successful activation", {
  skip_on_cran()
  expect_equal(activate_model("iris_model"), list(model_name = "iris_model",
                                                  is_active = TRUE))
  expect_equal(activate_model("kyphosis_model"), list(model_name = "kyphosis_model",
                                                  is_active = TRUE))
})

test_that("activate_model() returns error if 'model_name' is not length one character vector", {
  skip_on_cran()
  expect_error(activate_model(c("iris_model", "kyphosis_model")))
  expect_error(activate_model(1:2))
})

test_that("activate_model() returns error if model name is unknown to the server", {
  skip_on_cran()
  expect_error(activate_model("unknown_model"), err_not_known, fixed = TRUE)
})
