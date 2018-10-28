#' Get PMML model metrics
#'
#' Gets memory metrics and prediction metrics for the given PMML model from Zementis Server.
#'
#' @param model_name Name of the PMML model whose metrics are fetched from the server.
#' @inheritParams get_models
#' @return A list with the following components:
#'  \itemize{
#'    \item \code{prediction_metrics} A data frame containing prediction-related
#'      metrics for \code{model_name}. The information contained in \code{prediction_metrics}
#'      differs between regression and classification models. If no predictions have
#'      been calculated for \code{model_name} thus far on Zementis Server,
#'      \code{prediction_metrics} won't be included in the response list.
#'    \item \code{memory_metrics} A data frame containing memory-related metrics
#'      for \code{model_name} expressed in MB.
#'  }
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
  metrics <- list()

  if ("predictionMetrics" %in% names(parsed)) {
    prediction_metrics <- purrr::flatten_dfc(parsed[["predictionMetrics"]])
    parsed[["predictionMetrics"]] <- NULL
    metrics[["prediction_metrics"]] <- prediction_metrics
  } else if ("predictionMetricsErrorMsg" %in% names(parsed)) {
    parsed[["predictionMetricsErrorMsg"]] <- NULL
  }

  memory_metrics <- purrr::flatten_dfc(parsed)
  metrics[["memory_metrics"]] <- memory_metrics
  metrics
}
