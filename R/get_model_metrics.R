#' Get PMML model metrics
#'
#' Gets memory metrics and prediction metrics for a deployed PMML model from Zementis Server.
#'
#' The HTTP endpoint accessed by \code{get_model_metrics()} is only available for Zementis Server 10.3 or higher.
#'
#' @param model_name Name of the PMML model whose metrics are fetched from the server.
#' @inheritParams get_models
#' @return A list with the following components:
#'  \itemize{
#'    \item \code{model_name} A length one character vector containing the \code{model_name}
#'    \item \code{prediction_metrics} A data frame containing prediction-related
#'      metrics for \code{model_name}. The information contained in \code{prediction_metrics}
#'      differs between regression and classification models.
#'    \item \code{memory_metrics} A data frame containing memory-related metrics
#'      for \code{model_name} expressed in MB.
#'  }
#' If no predictions have been calculated for \code{model_name} thus far on Zementis Server,
#' \code{prediction_metrics} won't be included in the response list.
#'
#' If the model is deactivated while \code{get_model_metrics()} is called, the return list
#' neither includes \code{memory_metrics} nor \code{prediction_metrics}.
#' @seealso \code{\link{upload_model}}, \code{\link{apply_model}}, \code{\link{apply_model_batch}}
#' @export
#'
#' @examples
#'  \dontrun{
#'    # Some prep work
#'    iris_lm <- lm(Sepal.Length ~ ., data = iris)
#'    iris_pmml <- pmml::pmml(iris_lm, model.name = "iris_model")
#'    upload_model(iris_pmml)
#'
#'    # only includes memory metrics
#'    get_model_metrics("iris_model")
#'
#'    apply_model_batch(iris[23:33, ], "iris_model")
#'
#'    # includes memory and prediction metrics
#'    get_model_metrics("iris_model")
#' }
get_model_metrics <- function(model_name, ...) {
  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector")
  }

  url <- paste(get_zementis_base_url(), "model", model_name, "metrics",
               sep = "/")
  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                     get_zementis_pwd()),
             httr::content_type_json(),
             httr::user_agent(get_useragent()),
             ...)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) %in% c(404)) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  parsed <- httr::content(response, as = "text", encoding = "UTF-8") %>%
   jsonlite::fromJSON()
  metrics <- list(model_name = model_name)

  if ("predictionMetrics" %in% names(parsed)) {
    prediction_metrics <- purrr::flatten_dfc(parsed[["predictionMetrics"]])
    parsed[["predictionMetrics"]] <- NULL
    metrics[["prediction_metrics"]] <- prediction_metrics
  } else if ("predictionMetricsErrorMsg" %in% names(parsed)) {
    parsed[["predictionMetricsErrorMsg"]] <- NULL
  }

  if (!"memoryMetricsErrorMsg" %in% names(parsed)) {
    memory_metrics <- purrr::flatten_dfc(parsed) %>%
      purrr::map_dfc(function(x){as.numeric(sub(" MB", "", x))})
    metrics[["memory_metrics"]] <- memory_metrics
  }
  metrics
}
